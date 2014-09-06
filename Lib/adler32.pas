unit Adler32;

Interface

{
  Adler32 for Pascal v1.0 (C) 1997 Allen Cheng.

  Adler32 is a checksum algorithm simular to CRC32, but is faster
  than CRC32. This pascal unit has been verified with the Java class
  file included with JDK v1.1

  Test Vectors: Input - $11 $22 $33
               Output - $00AD0067

  Written by Allen Cheng.
  For the latest version, please visit http://tcp.home.ml.org/
}

procedure Adler32Byte(var adler: LongInt; const b: Byte);

procedure Adler32Update(var adler: LongInt;
                        var buf;
                        const len: Integer);
function Adler32File(var f: file): LongInt;
function Adler32Buff(var buf; const len: Integer): LongInt;

Implementation
type
  ByteArray = Array[0..0] of Byte;

const
  BASE = 65521; {* largest prime smaller than 65536 *}

procedure Adler32Byte(var adler: LongInt; const b: Byte);
var
  s1,s2 : LongInt;
begin
  s1 := ((adler and $ffff) + b) mod BASE;
  s2 := (((adler shr 16) and $ffff) + s1) mod BASE;
  adler := (s2 shl 16) + s1;
end;
{
 Update a running Adler-32 checksum with the bytes buf[0..len-1]
 and return the updated checksum. The Adler-32 checksum should be
 initialized to 1.
}
procedure Adler32Update(var adler: LongInt;
                        var buf;
                        const len: Integer);
var
  s1        : LongInt;
  s2        : LongInt;
  n         : LongInt;  { Faster in FPK-Pascal.. and GNU-Pascal??}
begin
  s1 := adler and $ffff;
  s2 := (adler shr 16) and $ffff;

  for n := 0 to len-1 do
  begin
    s1 := (s1 + ByteArray(buf)[n]) mod BASE;
    s2 := (s2 + s1) mod BASE;
  end;

  adler := (s2 shl 16) + s1;
end;

{* Return the adler32 of a file *}

function Adler32File(var f: file): LongInt;
var
  adler : LongInt;
  i     : Cardinal;
  inb   : Array[0..2048] of Byte;
begin
  adler := 1;
  repeat
    BlockRead(f, inb, SizeOf(inb), i);
    if i<>0 then Adler32Update(adler, inb, i);
  until (i=0);
  Adler32File := adler;
end;

function Adler32Buff(var buf; const len: Integer): LongInt;
var
  adler : LongInt;
begin
  adler := 1;
  Adler32Update(adler, buf, len);
  Adler32Buff := adler;
end;

end.