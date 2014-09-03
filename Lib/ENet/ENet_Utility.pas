unit ENet_Utility;

{
  ENet - Reliable UDP networking library
  Delphi 7 DLL header by Chernov D. Dmitry aka Black Doomer
  Original file: utility.h
  16.08.2014
}

interface

//inline macros
function ENET_MAX( x, y: Integer|Int64|Single|Double|Extended ): Integer|Int64|Single|Double|Extended; // inline;
function ENET_MIN( x, y: Integer|Int64|Single|Double|Extended ): Integer|Int64|Single|Double|Extended; // inline;

implementation

function ENET_MAX;
   begin Result := Max(x, y);
     end;
function ENET_MIN;
   begin Result := Min(x, y);
     end;

end.
