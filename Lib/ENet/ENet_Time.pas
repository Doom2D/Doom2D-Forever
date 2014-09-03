unit ENet_Time;

{
  ENet - Reliable UDP networking library
  Delphi 7 DLL header by Chernov D. Dmitry aka Black Doomer
  Original file: time.h
  16.08.2014
}

interface

const
  ENET_TIME_OVERFLOW = 86400000;

//inline macros
function ENET_TIME_LESS( a, b: LongInt ): Boolean; // inline;
function ENET_TIME_GREATER( a, b: LongInt ): Boolean; // inline;

function ENET_TIME_LESS_EQUAL( a, b: LongInt ): Boolean; // inline;
function ENET_TIME_GREATER_EQUAL( a, b: LongInt ): Boolean; // inline;

function ENET_TIME_DIFFERENCE( a, b: LongInt ): LongInt; // inline;

implementation

function ENET_TIME_LESS;
   begin Result := a - b >= ENET_TIME_OVERFLOW;
     end;
function ENET_TIME_GREATER;
   begin Result := b - a >= ENET_TIME_OVERFLOW;
     end;

function ENET_TIME_LESS_EQUAL;
   begin Result := not ENET_TIME_GREATER( a, b );
     end;
function ENET_TIME_GREATER_EQUAL;
   begin Result := not ENET_TIME_LESS( a, b );
     end;

function ENET_TIME_DIFFERENCE;
   begin if a - b >= ENET_TIME_OVERFLOW then Result := b - a else Result := a - b;
     end;

end.
