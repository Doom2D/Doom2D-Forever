{$INCLUDE ../a_modes.inc}
uses
  SysUtils,
  binheap in '../binheap.pas';


var
  heap: TBinaryHeapInt;
begin
  writeln('================');
  heap := binHeapNewIntLess();
  heap.insert(666);
  heap.insert(42);
  heap.insert(69);
  heap.insert(-666);
  heap.insert(8);

  while (heap.count > 0) do
  begin
    writeln(heap.front);
    heap.popFront();
  end;

  heap.Free();

  writeln('================');
  heap := binHeapNewIntGreat();
  heap.insert(666);
  heap.insert(42);
  heap.insert(69);
  heap.insert(-666);
  heap.insert(8);

  while (heap.count > 0) do
  begin
    writeln(heap.front);
    heap.popFront();
  end;

  heap.Free();
end.
