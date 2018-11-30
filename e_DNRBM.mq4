//+------------------------------------------------------------------+
//|                                                      e_DNRBM.mq4 |
//|                                   Copyright 2016,Vlad Perervenko |
//|                                                 v_minkov@mail.ru |
//+------------------------------------------------------------------+
#property copyright "PA"
#property link      "PA"
#property version   "1.00"
#property strict
// 01.03.16
/**
* This code is released under Gnu General Public License (GPL) V3
* If you need a commercial license then send me an email.
*/

/**
* For this to use you need the following:
*  - install R  (www.r-project.org)
*  - install mt4Rb7.mqh and mt4Rb7.dll
*  - set RPATH below to point to your R installation
*  - (optional) download and run DebugView.exe
*  - (optional) set RDEBUG to 2 to view more debug info in DebugView
*/

// set this so that it points to your R installation. Do NOT remove the --no-save
#define RPATH "C:/Program Files/R/R-3.3.1/bin/i386/Rterm.exe --no-save"
#define RUN "source('C:/Users/paulo.conceicao/Documents/01/Data_science/Nueva carpeta/Biblio_forex/MQL4/analysis/NNET/DN_SRBM/e_DNRBM.r')"
//--- input parameters
input double Lots          = 0.1;
input double TakeProfit    = 50.0;
input double StopLoss      = 25.0;
input int    magic         = 54321;
input int    cor           = 3;  //Angle of the text printing
input int    n             = 34; //Number of periods to use for indicators calculation
input int    z             = 37; //Minimum price movement in dollars in zigzag
input bool   soft          = true; //softmax?
input double Kmin          = 10; //set minimum quality of prediction 
input int    limit         = 5000; //глубина первой загрузки истории
input color  cvet          = Brown;

#include <mt4Rb7.mqh>
//-------GlobVar--------------------------------------
int  k = 0,  len = 0, mag = 0, lim = 700;
string fileName = "", pref = WindowExpertName();
static string text = "", Op = "ERR";
double o[], hi[], lo[], clo[], pr = 0.0;
static double signal[], sig = 0.0;
double K = 0.0, Kmax = 0.0, maxDD = 0.0, TP = 0.0, SL = 0.0, TS = 0.0;

//+------------------------------------------------------------------+
//| Expert initialization function                                   |
//+------------------------------------------------------------------+
int init()
  {
   if(!IsDllsAllowed())
     {
      MessageBox("You need to turn on \'Allow DLL imports\'");
     }
   
//--- create timer
   EventSetTimer(2);
//--- Start Rterm -----------------
   StartR(RPATH);
   
//-------------------------------------------  
   string terminalDataPath = TerminalInfoString(TERMINAL_DATA_PATH);
   StringReplace(terminalDataPath, "\\", "/");
   string patch = StringConcatenate(terminalDataPath, "/MQL4/Files/");
   string tf = GetNameTF(Period());
   fileName = createFileName();
   Rx("first <- TRUE");
   Ri("n", n);
   Ri("z", z);
   RAssignBool(hR, "soft", soft);
   Rs("patch", patch);
   Rs("fS", fileName);
   Rs("sym", Symbol());
   Rs("tf", tf);
  
//--------------------------------------- 
   if(Digits == 5 || Digits == 3) k = 10; else  k = 1;
   TP = TakeProfit * k;
   SL = StopLoss * k;
   Ri("Dig", Digits);
   Rd("Kmin", Kmin);
   mag = magic + Period(); 
//-------CheckWorkR----------------------------------------
   if(!RIsRunning(hR)){
       Alert("Rterm crash!(init)");
       return(1);
   }
   return(INIT_SUCCEEDED);
  }
//+------------------------------------------------------------------+
//| Expert deinitialization function                                 |
//+------------------------------------------------------------------+
void deinit()
  {
//--- destroy timer
   EventKillTimer();
   ObjectDelete("res");
    ObjectsDeleteAll(0, OBJ_ARROW);
//---- close Server -------
   StopR();
  }
//+------------------------------------------------------------------+
//| Timer function                                                   |
//+------------------------------------------------------------------+
void OnTimer()
  {
   int i = 0;
   static datetime LastTime = 0;
   static bool get_sig = false;
   static bool first = true;
   if(!RIsRunning(hR))
     {
      Alert("Rterm crash!(OnTimer)");
      return;
     }
   if(IsTradeAllowed()==false) Alert("Trade is not allowed!");
//-----Calculation----------------------------------------------------
   if(!RIsBusy(hR) && LastTime != Time[0])
     { //..If R is not busy and (new bar or retrained)
      Print("1");
	   if(RGetBool(hR,"first")) lim = limit;
      // Resize arrays
      ArrayResize(o, lim);
      ArrayResize(hi, lim);
      ArrayResize(lo, lim);
      ArrayResize(clo, lim);
      // Fill the array with new data
      for(i = 0; i < lim; i++)
        {
         o[i]  = Open[i+1];
         hi[i] = High[i+1];
         lo[i] = Low[i+1];
         clo[i]= Close[i+1];

        }
      //--------Send data to Rterm--------------------------------------
      Rv("Open",o);
      Rv("High",hi);
      Rv("Low",lo);
      Rv("Close",clo);
      //--------Load and go without waiting for the end of calculations-------------------
      RExecuteAsync(hR, RUN);
      LastTime= Time[0];
      get_sig = true;
      Print("2");
    }//.
//-------Get the result prediction-----------------
     else if(RIsRunning(hR) && !RIsBusy(hR) && get_sig )
     {//If Rterm work, not busy, and the result is ready
         printf("2: %s\n",TimeToString(Time[0]));
         GetRes();
         printf("2: %g, %g\n",signal[0], signal[1]);
         sig = signal[0];
         if(sig == 1) {Op = "BUY";}
         if(sig == -1) {Op = "SELL";}
         if(sig == 0) Op = "Nothing";
         if(Op != "ERR") get_sig=false;
         draw_Sig(sig);
     }
   drawSig(signal);
   text = StringConcatenate("OP = ", Op, ";  ", 
                                   "K = ", DoubleToStr(K,0), ";  ",
                                   "Kmax = ", DoubleToStr(Kmax,0), ";  ",
                                   "maxDD = ", DoubleToStr(maxDD,0));
   SetLabelText("res", text, cvet, 50, 30, cor, 12);
//----------------------------------------------
   CheckForClose(Op, mag);
   CheckForOpen(Op, mag);
   return;
  }
//+------------------------------------------------------------------+

//+------------------------------------------------------------------+
//===== Function ====================================================
//----------------------------------------------------------------------
//----------------------------------------------------------------------
void GetRes()
  {
      len = Rgi("length(S)");
      ArrayResize(signal, len);
      Rgv("rev(S)", signal);
      K = ND(Rgd("K"), 0);
      Kmax = ND(Rgd("Kmax"), 0);
      maxDD = ND(Rgd("maxDD"), 0);
    
    }
//----------------------------------------------------------------------
double ND(double A,int d=-1)
  {
   if(d == -1) d = Digits;
   return(NormalizeDouble(A, d));
  }
//----------------------------------------------
string createFileName()
  {
   string name = WindowExpertName() + "_" +
                 Symbol() + "_" +
                 IntegerToString(Period()) +
                 ".RData";
   return(name);
  }
//+----------------------------------------------------------------------------+
//|  Authro    : Kim Igor V. aka KimIV,  http://www.kimiv.ru                   |
//+----------------------------------------------------------------------------+
//|  Version   : 01.09.2005                                                    |
//|  Description : Returns the timeframe name                                  |
//+----------------------------------------------------------------------------+
//|  Parameters:                                                               |
//|    TimeFrame - timeframe (number of seconds)      (0 - current timeframe)  |
//+----------------------------------------------------------------------------+
string GetNameTF(int TimeFrame = 0)
  {
   if(TimeFrame == 0) TimeFrame = Period();
   switch(TimeFrame)
     {
      case PERIOD_M1:  return("M1");
      case PERIOD_M5:  return("M5");
      case PERIOD_M15: return("M15");
      case PERIOD_M30: return("M30");
      case PERIOD_H1:  return("H1");
      case PERIOD_H4:  return("H4");
      case PERIOD_D1:  return("Daily");
      case PERIOD_W1:  return("Weekly");
      case PERIOD_MN1: return("Monthly");
      default:         return("Unknown Period");
     }
  }
//+----------------------------------------------------------------------------+
//|  Authro    : Kim Igor V. aka KimIV,  http://www.kimiv.ru                   |
//+----------------------------------------------------------------------------+
//|  Version   : 12.10.2007                                                    |
//|  Description : Placing a text label, object OBJ_LABEL.                   |
//+----------------------------------------------------------------------------+
//|  Parameters:                                                               |
//|    nm - object name                                                        |
//|    tx - text                                                               |
//|    cl - label color                                                        |
//|    xd - X component in pixels                                              |
//|    yd - Y component in pixels                                              |
//|    cr - number of the bound corner (0 - top left )                         |
//|                                     1 - top right                          |
//|                                     2 - bottom left                        |
//|                                     3 - bottom right )                     |
//|    fs - font size                  (9 - by default  )
//     font- default font "pas" "Arial", "akt" -  "Arial Black"                         |
//+----------------------------------------------------------------------------+
void SetLabelText(string nm, string tx, color cl, int xd, int yd,
                  int cr = 0, int fs = 9, string font = "pas", int window = 0)
  {
   if(ObjectFind(nm) < 0)
      ObjectCreate(nm, OBJ_LABEL, window, 0, 0);
   if(font == "pas") font = "Arial";
   if(font == "akt") font = "Arial Black";
   ObjectSetText(nm, tx, fs, font);
   ObjectSet(nm, OBJPROP_COLOR, cl);
   ObjectSet(nm, OBJPROP_XDISTANCE, xd);
   ObjectSet(nm, OBJPROP_YDISTANCE, yd);
   ObjectSet(nm, OBJPROP_CORNER, cr);

  }
//+------------------------------------------------------------------+
//-------------------------------------------------------------------
void draw_Sig(double &s)//
  {
   double b = s; pr = 0.0;
   int code = 119;
   color cv = Gold;
   string var1=TimeToStr(TimeCurrent(),TIME_DATE|TIME_SECONDS);
   string txt=StringConcatenate("sig",var1);
   if(b == 1)  {cv = clrAqua; pr = Low[1];}
   if(b == -1) {cv = clrPlum; pr = High[1];}
   //if(b == 0)  { cv = Red; }
   SetArrowT(txt, code, cv, Time[0], pr, 1, 0);
   return;
  }
//----------------------------------------------------------------------
void drawSig(double &s[])
  {
   for(int i=0; i < ArraySize(s)-1; i++)
     {
      string txt=StringConcatenate("sig",IntegerToString(i));
      ObjectDelete(txt);
     }

   for(int i=0; i < ArraySize(s)-1; i++)
     {
      double b = s[i];
      int code = 0;
      color cv = Gold;
      if(s[i]!=s[i+1])
        {
         string txt=StringConcatenate("sig",IntegerToString(i));
         if(b == 1)  {code = 233; cv = Blue;}
         if(b == -1) {code = 234; cv = Red; }
         //if(b == 0)  {code = 251; cv = Red; }
         SetArrowT(txt,code,cv,Time[i] ,Open[i],1,0);
        }
     }
   return;
  }
//+----------------------------------------------------------------------------+
void SetArrowT(string nm="",int cod=0,color cl=0,datetime t1=0,double p1=0,
               int sz=0,int window=0) //..
  {
   if(nm=="") nm=DoubleToStr(Time[0],0);
   if(t1 <= 0) t1 = Time[0];
   if(p1 <= 0) p1 = Bid;
   if(ObjectFind(nm)<0)
      ObjectCreate(nm,OBJ_ARROW,window,0,0);
   ObjectSet(nm,OBJPROP_TIME1,t1);
   ObjectSet(nm,OBJPROP_PRICE1,p1);
   ObjectSet(nm,OBJPROP_ARROWCODE,cod);
   ObjectSet(nm,OBJPROP_COLOR,cl);
   ObjectSet(nm,OBJPROP_WIDTH,sz);
  }//.
//+------------------------------------------------------------------+
//| Calculate open positions                                         |
//+------------------------------------------------------------------+
int CalculateCurrentOrders(int magi)
  {
   int buys = 0, sells = 0;
//----
   for(int i = 0; i < OrdersTotal(); i++)
     {
      if(OrderSelect(i,SELECT_BY_POS,MODE_TRADES) == false) continue;
      if(OrderSymbol() == Symbol() && OrderMagicNumber() == magi)
        {
         if(OrderType() == OP_BUY) buys++;
         if(OrderType() == OP_SELL) sells++;
        }
     }
//----  
   if(buys > 0) return(buys);
   else       return(sells);
  }
//----------------------------------------------------------------------
//+------------------------------------------------------------------+
//| Check for close order conditions                                 |
//+------------------------------------------------------------------+

bool CheckForClose(string op, int magi)
  {

//----
   for(int i = 0; i < OrdersTotal(); i++)
     {
      if(OrderSelect(i,SELECT_BY_POS,MODE_TRADES) == false) continue;
      if(OrderMagicNumber() != magi || OrderSymbol() != Symbol()) continue;
      //---- check order type 
      if((OrderType() == OP_BUY) && (op == "SELL" || op == "CLOSE" || op == "ERR"))
        {
         if(!OrderClose(OrderTicket(), OrderLots(), Bid, 3, White))
           {
            Alert("Failed to close position"+Symbol());
            return(false);
           }
         else return(true);
        }
      if(( OrderType() == OP_SELL) && (op == "BUY" || op == "CLOSE" || op == "ERR"))
        {
         if(!OrderClose(OrderTicket(), OrderLots(), Ask, 3, White))
           {
            Alert("Failed to close position"+Symbol());
            return(false);
           }
         else return(true);
        }
     }
//----
   return(false);
  }
//+------------------------------------------------------------------+
//| Check for open order conditions                                  |
//+------------------------------------------------------------------+
bool CheckForOpen(string op, int magi)
  {
   int ticket = -1;
   double sl = 0, tp = 0;
   int pos = CalculateCurrentOrders(magi);//Are there open positions?
   if(pos > 0) return(true);             //If permissible number is exceeded, exit
//---- sell conditions
   if(op == "SELL")
     {
      RefreshRates();
      //if(TP > 0)
         tp = ND((Bid - TP * Point));
     // if(SL > 0)
         sl = ND((Bid + SL * Point));
      ticket=OrderSend(Symbol(), OP_SELL, Lots, Bid, 3, sl, tp,
                       "DNRBM", magi, 0, Red);
      if(ticket > 0)
        {
         if(OrderSelect(ticket, SELECT_BY_TICKET, MODE_TRADES))
           {
            Print("SELL order opened : ", OrderOpenPrice());
            return(true);
           }
           } else {
         Print("Error opening SELL order : ", GetLastError());
         Alert("Failed to open the SELL position " + Symbol());
         return(false);
        }
     }

//---- buy conditions
   if(op == "BUY")
     {
      RefreshRates();
     // if(TP > 0)
         tp = ND((Ask + TP * Point));
      //if(SL > 0)
         sl = ND((Ask - SL * Point));
      ticket = OrderSend(Symbol(), OP_BUY, Lots, Ask, 3, sl, tp,
                       "DNRBM", magi, 0, Blue);
      if(ticket > 0)
        {
         if(OrderSelect(ticket, SELECT_BY_TICKET, MODE_TRADES))
           {
            Print("BUY order opened : ", OrderOpenPrice());
            return(true);
           }
           } else {
         Print("Error opening BUY order : ", GetLastError());
         Alert("Failed to open the BUY position " + Symbol());
         return(false);
        }
     }

//----
   return(false);
  }
//+------------------------------------------------------------------+
