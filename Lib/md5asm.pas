unit md5asm;

// MD5 hash calculation converted by Grom PE
// 18 Feb 2008 - initial version
// 10 Mar 2008 - optimized Byte2Hex
// 07 Nov 2008 - fix MD5 for files larger than 4 Gb by Xplorer

interface

uses Windows;

type
  PMD5Digest=^TMD5Digest;
  TMD5Digest=record
   case integer of
     0: (A,B,C,D:longword);
     1: (v:array [0..15] of byte);
  end;

type
  TArray4Longword=array [0..3] of longword;
  TArray2Longword=array [0..1] of longword;

  PArray64Byte=^TArray64Byte;
  TArray64Byte=array [0..63] of byte;

  PByteArray=^TByteArray;
  TByteArray=array [0..0] of byte;

  PLongwordArray=^TLongwordArray;
  TLongwordArray=array [0..0] of longword;

  PMD5Context=^TMD5Context;
  TMD5Context=record
    state:TArray4Longword;
    count:TArray2Longword;
    buffer:TArray64Byte;
  end;

function MD5String(const S:string):TMD5Digest;
function MD5File(const FileName:string):TMD5Digest;
function MD5Buffer(const Buffer; Size:integer):TMD5Digest;
function MD5ToStr(const Digest:TMD5Digest):string;
function MD5Compare(const Digest1,Digest2:TMD5Digest):boolean;

procedure MD5Init(var Context:TMD5Context);
procedure MD5Final(var Context:TMD5Context; var Digest:TMD5Digest);
procedure MD5Update(var Context:TMD5Context; Input:PByteArray; InputLen:longword);

implementation

procedure MD5Transform;
asm
  mov  edx, dword [12+ebp]
  mov  eax, dword [ebp]

  push esi
  push ebx

  mov  ecx, dword [8+ebp]
  mov  esi, dword [4+ebp]


  mov  ebx, edx
  add  eax, dword [edi]
  xor  ebx, ecx
  and  ebx, esi
  xor  ebx, edx
  add  edx, dword [4+edi]
  lea  eax, [0d76aa478h+ebx+eax]
  mov  ebx, ecx
  rol  eax, 7
  add  eax, esi

  xor  ebx, esi
  and  ebx, eax
  xor  ebx, ecx
  lea  edx, [0e8c7b756h+ebx+edx]
  add  ecx, dword [8+edi]
  rol  edx, 12
  mov  ebx, esi
  add  edx, eax

  xor  ebx, eax
  and  ebx, edx
  xor  ebx, esi
  lea  ecx, [0242070dbh+ebx+ecx]
  add  esi, dword [12+edi]
  rol  ecx, 17
  mov  ebx, eax
  add  ecx, edx

  xor  ebx, edx
  and  ebx, ecx
  xor  ebx, eax
  lea  esi, [0c1bdceeeh+ebx+esi]
  add  eax, dword [16+edi]
  rol  esi, 22
  mov  ebx, edx
  add  esi, ecx

  xor  ebx, ecx
  and  ebx, esi
  xor  ebx, edx
  lea  eax, [0f57c0fafh+ebx+eax]
  add  edx, dword [20+edi]
  rol  eax, 7
  mov  ebx, ecx
  add  eax, esi

  xor  ebx, esi
  and  ebx, eax
  xor  ebx, ecx
  lea  edx, [04787c62ah+ebx+edx]
  add  ecx, dword [24+edi]
  rol  edx, 12
  mov  ebx, esi
  add  edx, eax

  xor  ebx, eax
  and  ebx, edx
  xor  ebx, esi
  lea  ecx, [0a8304613h+ebx+ecx]
  add  esi, dword [28+edi]
  rol  ecx, 17
  mov  ebx, eax
  add  ecx, edx

  xor  ebx, edx
  and  ebx, ecx
  xor  ebx, eax
  lea  esi, [0fd469501h+ebx+esi]
  add  eax, dword [32+edi]
  rol  esi, 22
  mov  ebx, edx
  add  esi, ecx

  xor  ebx, ecx
  and  ebx, esi
  xor  ebx, edx
  lea  eax, [0698098d8h+ebx+eax]
  add  edx, dword [36+edi]
  rol  eax, 7
  mov  ebx, ecx
  add  eax, esi

  xor  ebx, esi
  and  ebx, eax
  xor  ebx, ecx
  lea  edx, [08b44f7afh+ebx+edx]
  add  ecx, dword [40+edi]
  rol  edx, 12
  mov  ebx, esi
  add  edx, eax

  xor  ebx, eax
  and  ebx, edx
  xor  ebx, esi
  lea  ecx, [0ffff5bb1h+ebx+ecx]
  add  esi, dword [44+edi]
  rol  ecx, 17
  mov  ebx, eax
  add  ecx, edx

  xor  ebx, edx
  and  ebx, ecx
  xor  ebx, eax
  lea  esi, [0895cd7beh+ebx+esi]
  add  eax, dword [48+edi]
  rol  esi, 22
  mov  ebx, edx
  add  esi, ecx

  xor  ebx, ecx
  and  ebx, esi
  xor  ebx, edx
  lea  eax, [06b901122h+ebx+eax]
  add  edx, dword [52+edi]
  rol  eax, 7
  mov  ebx, ecx
  add  eax, esi

  xor  ebx, esi
  and  ebx, eax
  xor  ebx, ecx
  lea  edx, [0fd987193h+ebx+edx]
  add  ecx, dword [56+edi]
  rol  edx, 12
  mov  ebx, esi
  add  edx, eax

  xor  ebx, eax
  and  ebx, edx
  xor  ebx, esi
  lea  ecx, [0a679438eh+ebx+ecx]
  add  esi, dword [60+edi]
  rol  ecx, 17
  mov  ebx, eax
  add  ecx, edx

  xor  ebx, edx
  and  ebx, ecx
  xor  ebx, eax
  lea  esi, [049b40821h+ebx+esi]
  rol  esi, 22
  mov  ebx, ecx
  add  esi, ecx


  add  eax, dword [4+edi]
  xor  ebx, esi
  and  ebx, edx
  xor  ebx, ecx
  lea  eax, [0f61e2562h+ebx+eax]
  rol  eax, 5
  mov  ebx, esi
  add  eax, esi

  add  edx, dword [24+edi]
  xor  ebx, eax
  and  ebx, ecx
  xor  ebx, esi
  lea  edx, [0c040b340h+ebx+edx]
  rol  edx, 9
  mov  ebx, eax
  add  edx, eax

  add  ecx, dword [44+edi]
  xor  ebx, edx
  and  ebx, esi
  xor  ebx, eax
  lea  ecx, [0265e5a51h+ebx+ecx]
  rol  ecx, 14
  mov  ebx, edx
  add  ecx, edx

  add  esi, dword [edi]
  xor  ebx, ecx
  and  ebx, eax
  xor  ebx, edx
  lea  esi, [0e9b6c7aah+ebx+esi]
  rol  esi, 20
  mov  ebx, ecx
  add  esi, ecx

  add  eax, dword [20+edi]
  xor  ebx, esi
  and  ebx, edx
  xor  ebx, ecx
  lea  eax, [0d62f105dh+ebx+eax]
  rol  eax, 5
  mov  ebx, esi
  add  eax, esi

  add  edx, dword [40+edi]
  xor  ebx, eax
  and  ebx, ecx
  xor  ebx, esi
  lea  edx, [002441453h+ebx+edx]
  rol  edx, 9
  mov  ebx, eax
  add  edx, eax

  add  ecx, dword [60+edi]
  xor  ebx, edx
  and  ebx, esi
  xor  ebx, eax
  lea  ecx, [0d8a1e681h+ebx+ecx]
  rol  ecx, 14
  mov  ebx, edx
  add  ecx, edx

  add  esi, dword [16+edi]
  xor  ebx, ecx
  and  ebx, eax
  xor  ebx, edx
  lea  esi, [0e7d3fbc8h+ebx+esi]
  rol  esi, 20
  mov  ebx, ecx
  add  esi, ecx

  add  eax, dword [36+edi]
  xor  ebx, esi
  and  ebx, edx
  xor  ebx, ecx
  lea  eax, [021e1cde6h+ebx+eax]
  rol  eax, 5
  mov  ebx, esi
  add  eax, esi

  add  edx, dword [56+edi]
  xor  ebx, eax
  and  ebx, ecx
  xor  ebx, esi
  lea  edx, [0c33707d6h+ebx+edx]
  rol  edx, 9
  mov  ebx, eax
  add  edx, eax

  add  ecx, dword [12+edi]
  xor  ebx, edx
  and  ebx, esi
  xor  ebx, eax
  lea  ecx, [0f4d50d87h+ebx+ecx]
  rol  ecx, 14
  mov  ebx, edx
  add  ecx, edx

  add  esi, dword [32+edi]
  xor  ebx, ecx
  and  ebx, eax
  xor  ebx, edx
  lea  esi, [0455a14edh+ebx+esi]
  rol  esi, 20
  mov  ebx, ecx
  add  esi, ecx

  add  eax, dword [52+edi]
  xor  ebx, esi
  and  ebx, edx
  xor  ebx, ecx
  lea  eax, [0a9e3e905h+ebx+eax]
  rol  eax, 5
  mov  ebx, esi
  add  eax, esi

  add  edx, dword [8+edi]
  xor  ebx, eax
  and  ebx, ecx
  xor  ebx, esi
  lea  edx, [0fcefa3f8h+ebx+edx]
  rol  edx, 9
  mov  ebx, eax
  add  edx, eax

  add  ecx, dword [28+edi]
  xor  ebx, edx
  and  ebx, esi
  xor  ebx, eax
  lea  ecx, [0676f02d9h+ebx+ecx]
  rol  ecx, 14
  mov  ebx, edx
  add  ecx, edx

  add  esi, dword [48+edi]
  xor  ebx, ecx
  and  ebx, eax
  xor  ebx, edx
  lea  esi, [08d2a4c8ah+ebx+esi]
  rol  esi, 20
  add  esi, ecx
  mov  ebx, edx


  xor  ebx, ecx
  add  eax, dword [20+edi]
  xor  ebx, esi
  lea  eax, [0fffa3942h+ebx+eax]
  rol  eax, 4
  mov  ebx, ecx
  add  eax, esi

  add  edx, dword [32+edi]
  xor  ebx, esi
  xor  ebx, eax
  lea  edx, [08771f681h+ebx+edx]
  rol  edx, 11
  mov  ebx, esi
  add  edx, eax

  add  ecx, dword [44+edi]
  xor  ebx, eax
  xor  ebx, edx
  lea  ecx, [06d9d6122h+ebx+ecx]
  rol  ecx, 16
  mov  ebx, eax
  add  ecx, edx

  add  esi, dword [56+edi]
  xor  ebx, edx
  xor  ebx, ecx
  lea  esi, [0fde5380ch+ebx+esi]
  rol  esi, 23
  mov  ebx, edx
  add  esi, ecx

  add  eax, dword [4+edi]
  xor  ebx, ecx
  xor  ebx, esi
  lea  eax, [0a4beea44h+ebx+eax]
  rol  eax, 4
  mov  ebx, ecx
  add  eax, esi

  add  edx, dword [16+edi]
  xor  ebx, esi
  xor  ebx, eax
  lea  edx, [04bdecfa9h+ebx+edx]
  rol  edx, 11
  mov  ebx, esi
  add  edx, eax

  add  ecx, dword [28+edi]
  xor  ebx, eax
  xor  ebx, edx
  lea  ecx, [0f6bb4b60h+ebx+ecx]
  rol  ecx, 16
  mov  ebx, eax
  add  ecx, edx

  add  esi, dword [40+edi]
  xor  ebx, edx
  xor  ebx, ecx
  lea  esi, [0bebfbc70h+ebx+esi]
  rol  esi, 23
  mov  ebx, edx
  add  esi, ecx

  add  eax, dword [52+edi]
  xor  ebx, ecx
  xor  ebx, esi
  lea  eax, [0289b7ec6h+ebx+eax]
  rol  eax, 4
  mov  ebx, ecx
  add  eax, esi

  add  edx, dword [edi]
  xor  ebx, esi
  xor  ebx, eax
  lea  edx, [0eaa127fah+ebx+edx]
  rol  edx, 11
  mov  ebx, esi
  add  edx, eax

  add  ecx, dword [12+edi]
  xor  ebx, eax
  xor  ebx, edx
  lea  ecx, [0d4ef3085h+ebx+ecx]
  rol  ecx, 16
  mov  ebx, eax
  add  ecx, edx

  add  esi, dword [24+edi]
  xor  ebx, edx
  xor  ebx, ecx
  lea  esi, [004881d05h+ebx+esi]
  rol  esi, 23
  mov  ebx, edx
  add  esi, ecx

  add  eax, dword [36+edi]
  xor  ebx, ecx
  xor  ebx, esi
  lea  eax, [0d9d4d039h+ebx+eax]
  rol  eax, 4
  mov  ebx, ecx
  add  eax, esi

  add  edx, dword [48+edi]
  xor  ebx, esi
  xor  ebx, eax
  lea  edx, [0e6db99e5h+ebx+edx]
  rol  edx, 11
  mov  ebx, esi
  add  edx, eax

  add  ecx, dword [60+edi]
  xor  ebx, eax
  xor  ebx, edx
  lea  ecx, [01fa27cf8h+ebx+ecx]
  rol  ecx, 16
  mov  ebx, eax
  add  ecx, edx

  add  esi, dword [8+edi]
  xor  ebx, edx
  xor  ebx, ecx
  lea  esi, [0c4ac5665h+ebx+esi]
  rol  esi, 23
  mov  ebx, edx
  add  esi, ecx


  add  eax, dword [edi]
  not  ebx
  or   ebx, esi
  xor  ebx, ecx
  lea  eax, [0f4292244h+ebx+eax]
  rol  eax, 6
  mov  ebx, ecx
  add  eax, esi

  add  edx, dword [28+edi]
  not  ebx
  or   ebx, eax
  xor  ebx, esi
  lea  edx, [0432aff97h+ebx+edx]
  rol  edx, 10
  mov  ebx, esi
  add  edx, eax

  add  ecx, dword [56+edi]
  not  ebx
  or   ebx, edx
  xor  ebx, eax
  lea  ecx, [0ab9423a7h+ebx+ecx]
  rol  ecx, 15
  mov  ebx, eax
  add  ecx, edx

  add  esi, dword [20+edi]
  not  ebx
  or   ebx, ecx
  xor  ebx, edx
  lea  esi, [0fc93a039h+ebx+esi]
  rol  esi, 21
  mov  ebx, edx
  add  esi, ecx

  add  eax, dword [48+edi]
  not  ebx
  or   ebx, esi
  xor  ebx, ecx
  lea  eax, [0655b59c3h+ebx+eax]
  rol  eax, 6
  mov  ebx, ecx
  add  eax, esi

  add  edx, dword [12+edi]
  not  ebx
  or   ebx, eax
  xor  ebx, esi
  lea  edx, [08f0ccc92h+ebx+edx]
  rol  edx, 10
  mov  ebx, esi
  add  edx, eax

  add  ecx, dword [40+edi]
  not  ebx
  or   ebx, edx
  xor  ebx, eax
  lea  ecx, [0ffeff47dh+ebx+ecx]
  rol  ecx, 15
  mov  ebx, eax
  add  ecx, edx

  add  esi, dword [4+edi]
  not  ebx
  or   ebx, ecx
  xor  ebx, edx
  lea  esi, [085845dd1h+ebx+esi]
  rol  esi, 21
  mov  ebx, edx
  add  esi, ecx

  add  eax, dword [32+edi]
  not  ebx
  or   ebx, esi
  xor  ebx, ecx
  lea  eax, [06fa87e4fh+ebx+eax]
  rol  eax, 6
  mov  ebx, ecx
  add  eax, esi

  add  edx, dword [60+edi]
  not  ebx
  or   ebx, eax
  xor  ebx, esi
  lea  edx, [0fe2ce6e0h+ebx+edx]
  rol  edx, 10
  mov  ebx, esi
  add  edx, eax

  add  ecx, dword [24+edi]
  not  ebx
  or   ebx, edx
  xor  ebx, eax
  lea  ecx, [0a3014314h+ebx+ecx]
  rol  ecx, 15
  mov  ebx, eax
  add  ecx, edx

  add  esi, dword [52+edi]
  not  ebx
  or   ebx, ecx
  xor  ebx, edx
  lea  esi, [04e0811a1h+ebx+esi]
  rol  esi, 21
  mov  ebx, edx
  add  esi, ecx

  add  eax, dword [16+edi]
  not  ebx
  or   ebx, esi
  xor  ebx, ecx
  lea  eax, [0f7537e82h+ebx+eax]
  rol  eax, 6
  mov  ebx, ecx
  add  eax, esi

  add  dword [ebp], eax

  add  edx, dword [44+edi]
  not  ebx
  or   ebx, eax
  xor  ebx, esi
  lea  edx, [0bd3af235h+ebx+edx]
  rol  edx, 10
  mov  ebx, esi
  add  edx, eax

  add  dword [12+ebp], edx

  add  ecx, dword [8+edi]
  not  ebx
  or   ebx, edx
  xor  ebx, eax
  lea  ecx, [02ad7d2bbh+ebx+ecx]
  rol  ecx, 15
  mov  ebx, eax
  add  ecx, edx

  add  dword [8+ebp], ecx

  add  esi, dword [36+edi]
  not  ebx
  or   ebx, ecx
  xor  ebx, edx
  lea  esi, [0eb86d391h+ebx+esi]
  rol  esi, 21
  add  esi, ecx

  pop  ebx

  add  dword [4+ebp], esi

  pop  esi
end;

procedure MD5Init(var Context:TMD5Context);
asm
  mov  dword [eax], 067452301h
  mov  dword [4+eax], 0efcdab89h
  mov  dword [8+eax], 098badcfeh
  mov  dword [12+eax], 010325476h
  mov  dword [16+eax], 0
  mov  dword [84+eax], 0
end;

procedure MD5Update(var Context:TMD5Context; Input:PByteArray; InputLen:longword);
asm
  push ebp
  push ebx
  push esi
  push edi

  mov  ebp, eax
  mov  esi, ecx
  lea  edi, [20+ebp]
  mov  ebx, edx

  mov  ecx, [16+ebp]
  and  ecx, $3F

  add  dword [16+ebp], esi
  adc  dword [84+ebp], 0

  test esi, esi
  je   @@loop1end
  test ecx, ecx
  je   @@loop1end
@@loop1:
  mov  al, byte [ebx]
  inc  ebx
  mov  byte [edi+ecx], al
  inc  ecx
  cmp  ecx, 64
  je   @@cont1
  dec  esi
  jne  @@loop1
  jmp  @@loop1end
@@cont1:
  call MD5Transform
  xor  ecx, ecx
  dec  esi

@@loop1end:
  mov  edi, ebx
  mov  ebx, ecx
  cmp  esi, 64
  jnae @@loop2end

@@loop2:
  call MD5Transform
  sub  esi, 64
  add  edi, 64
  cmp  esi, 64
  jnb  @@loop2

@@loop2end:
  mov  edx, esi
  shr  esi, 2
  je   @@loop3end

@@loop3:
  mov  eax, dword [edi]
  add  edi, 4
  mov  dword [20+ebp+ebx], eax
  add  ebx, 4
  dec  esi
  jne  @@loop3

@@loop3end:
  and  edx, 3
  je   @@end

@@loop4:
  mov  al, byte [edi]
  inc  edi
  mov  byte [20+ebp+ebx], al
  inc  ebx
  dec  edx
  jne  @@loop4

@@end:
  pop  edi
  pop  esi

  pop  ebx
  pop  ebp
end;

procedure MD5Final(var Context:TMD5Context; var Digest:TMD5Digest);
asm
  push ebp
  push esi
  push edi
  push ebx

  mov  ebp, eax
  mov  esi, edx
  lea  edi, [20+ebp]

  mov  ebx, [16+ebp]
  and  ebx, $3F
  inc  ebx
  mov  eax, ebx
  mov  byte [-1+edi+ebx], 128
  and  eax, 3
  je   @@loop1aend

@@loop1a:
  mov  byte [edi+ebx], 0
  inc  eax
  inc  ebx
  cmp  eax, 4
  jne  @@loop1a

@@loop1aend:
  shr  ebx, 2
  jmp  @@loop1bbeg

@@loop1b:
  mov  dword [edi+ebx*4], 0
  inc  ebx

@@loop1bbeg:
  cmp  ebx, 14
  je   @@loop1end
  cmp  ebx, 16
  jne  @@loop1b
  xor  ebx, ebx
  call MD5Transform
  jmp  @@loop1b

@@loop1end:
  mov  edx, dword [84+ebp]
  mov  eax, dword [16+ebp]
  mov  ecx, eax
  shl  ecx, 3
  shrd eax, edx, 29
  mov  dword [76+ebp], ecx
  mov  dword [80+ebp], eax
  call MD5Transform

  mov  eax, dword [ebp]
  mov  dword [esi], eax
  mov  eax, dword [ebp+4]
  mov  dword [esi+4], eax
  mov  eax, dword [ebp+8]
  mov  dword [esi+8], eax
  mov  eax, dword [ebp+12]
  mov  dword [esi+12], eax

  pop  ebx
  pop  edi

  pop  esi
  pop  ebp
end;

procedure Byte2Hex(b:byte; ch2:pchar);
asm
  mov  ah,al
  and  al,$0F
  cmp  al,10
  sbb  al,$69
  das
  or   al,$20
  xchg al,ah
  shr  al,$04
  cmp  al,10
  sbb  al,$69
  das
  or   al,$20
  mov  [edx],ax
end;

function MD5ToStr(const Digest:TMD5Digest):string;
var i:integer;
begin
  SetLength(Result,32);
  for i:=0 to 15 do Byte2Hex(Digest.v[i],@Result[i*2+1]);
end;

function MD5String(const S:string):TMD5Digest;
begin
  Result:=MD5Buffer(pchar(S)^,Length(S));
end;

function MD5File(const FileName:string):TMD5Digest;
const
  CHUNK_SIZE=4*1024*1024; // 4 mb is optimal for most systems
var
    FileHandle, MapHandle: THandle;
        FSize, FOffs, Tmp: ULARGE_INTEGER;
              ViewPointer: Pointer;
             ProcessBytes: longword;
                  Context: TMD5Context;
begin
  MD5Init(Context);
  FileHandle:=CreateFile(pchar(FileName), GENERIC_READ, FILE_SHARE_READ,
    nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if FileHandle <> INVALID_HANDLE_VALUE then
  begin
    MapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
    if MapHandle <> 0 then
    begin
      FSize.LowPart := GetFileSize(FileHandle, @FSize.HighPart);
      FOffs.QuadPart := 0;
      while FOffs.QuadPart < FSize.QuadPart do
      begin
        ProcessBytes := CHUNK_SIZE;
        Tmp.QuadPart := FSize.QuadPart - FOffs.QuadPart;
        if Tmp.QuadPart < CHUNK_SIZE then ProcessBytes := Tmp.LowPart;
        ViewPointer := MapViewOfFile(MapHandle, FILE_MAP_READ,
          FOffs.HighPart, FOffs.LowPart, ProcessBytes);
        if ViewPointer = nil then Break;
        MD5Update(Context, ViewPointer, ProcessBytes);
        UnmapViewOfFile(ViewPointer);
        Inc(FOffs.QuadPart, CHUNK_SIZE);
      end;
      CloseHandle(MapHandle);
    end;
    CloseHandle(FileHandle);
  end;
  MD5Final(Context, Result);
end;

function MD5Buffer(const Buffer; Size:integer):TMD5Digest;
var Context:TMD5Context;
begin
  MD5Init(Context);
  MD5Update(Context,PByteArray(@Buffer),Size);
  MD5Final(Context, Result);
end;

function MD5Compare(const Digest1,Digest2:TMD5Digest):boolean;
begin
  Result:=false;
  if Digest1.A<>Digest2.A then Exit;
  if Digest1.B<>Digest2.B then Exit;
  if Digest1.C<>Digest2.C then Exit;
  if Digest1.D<>Digest2.D then Exit;
  Result:=true;
end;

end.
