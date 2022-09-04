unit UDThreaded;

{$mode objfpc}{$H+}

interface

uses Classes, cubedefs, facecube;

type

  threadWrapper = class(TThread)
  private
    x, y, max: integer; // Koordinaten des Clusters
    fc: faceletcube;
  protected
    procedure Execute; override;
  public
    constructor Create(i, j, maxDepth: integer; ffc: faceletcube);
  end;

  makeUDCenter = class(TThread)
  private
    // Achse auf der der 1. Zug stattfindet
    ax: integer; // 1. Drehung hat index 3*ax bis 3*ax+2
    x, y, max: integer; // Koordinaten des Clusters
    fc: faceletcube;
  protected
    procedure Execute; override;
    constructor Create(a: integer; i, j, maxDepth: integer; flc: faceletcube);
    procedure SearchUDCenter(ccx, slx, ccy, sly, togo: integer);
  end;

  makeUDAll = class(TThread)
  private
    procedure showstuff;
    procedure showstuff2;
    procedure showstuff3;
    procedure showstuff4;
    procedure showstuff5;
    procedure showstuff6;
    procedure printApply;
  var
    ii, jj: integer;
    fcfc: faceletcube;
  protected
    procedure Execute; override;
  public
  end;

  blueprint = class(TThread)
  private
    ax: Axis;
    procedure showstuff;
  protected
    procedure Execute; override;
    constructor Create(a: Axis); // Achse auf der der 1. Zug stattfindet
  end;

implementation

uses main, SysUtils, Windows, Generics.Collections, Forms, phase1_tables;

const
  maxThreads = 18; // Sollte ein Teiler von 54 sein

var
  tud: array [0 .. maxThreads - 1] of makeUDCenter; // Globale Variable

constructor threadWrapper.Create(i, j, maxDepth: integer; ffc: faceletcube);
begin
  inherited Create(False); // gleich starten
  x := i;
  y := j;
  max := maxDepth;
  fc := ffc;
  // false: Ausführung beginnt sofort. true: starter erst mit resume
end;

procedure threadWrapper.Execute;
var
  aa: integer; // 1. Drehung hat index 3*aa bis 3*aa+2
  success: boolean;
  handles: array [0 .. maxThreads - 1] of THandle;
  k: integer;
begin

  for aa := 0 to maxThreads - 1 do
  begin
    tud[aa] := makeUDCenter.Create(aa, x, y, max, fc);
    tud[aa].FreeOnTerminate := False;
    tud[aa].Priority := tpLowest;
    handles[aa] := tud[aa].Handle;
  end;
  for aa := 0 to maxThreads - 1 do

  begin
    tud[aa].Resume;
  end;

  WaitForMultipleObjects(maxThreads, @handles, True, INFINITE);
  // true: wartet, bis alle terminiert sind

  Form1.fcube.mvIdx := -1; // signalisiert keine Lösung
  for aa := 0 to maxThreads - 1 do
    if tud[aa].fc.found = True then
    begin
      Form1.fcube.mvIdx := tud[aa].fc.mvIdx; // dirty hack
      for k := 0 to tud[aa].fc.mvIdx - 1 do
        Form1.fcube.fxymoves[k] := tud[aa].fc.fxymoves[k];
      break;
    end;

  // aufräumen
  for aa := 0 to maxThreads - 1 do
  begin
    tud[aa].fc.Free;
    tud[aa].Free;
  end;

end;

constructor makeUDCenter.Create(a: integer; i, j, maxDepth: integer; flc: faceletcube);
begin
  inherited Create(True); // nicht starten
  ax := a;
  x := i;
  y := j;
  max := maxDepth;
  fc := faceletcube.Create(flc);
end;

procedure makeUDCenter.Execute;
var
  idx, togo: integer;
begin
  togo := 0;
  fc.found := False;
  for idx := Low(fc.fxymoves) to High(fc.fxymoves) do
    fc.fxymoves[idx] := InitMove;

  while (fc.found = False) and not Terminated do
  begin
    if togo > max then
      Exit;
    fc.mvIdx := 0; // 1. free place in fxymoves
    //Phase1Brick256Coord(x,y)/(y,x) should be 0, which means solved
    SearchUDCenter(fc.Phase1CenterCoord(x, y), fc.Phase1Brick256Coord(x, y),
      fc.Phase1CenterCoord(y, x), fc.Phase1Brick256Coord(y, x), togo);
    Inc(togo);
  end;
end;

procedure makeUDCenter.SearchUDCenter(ccx, slx, ccy, sly, togo: integer);
var
  mv: moves;
  sc1: SymCoord32;
  syms: UInt8;
  n, altccy, altslx, key: integer;

  newccx, newccy, newslx, newsly: integer;
  aa: integer;
begin

  if togo = 10 then
  begin

    sc1 := UDCentCoordToSymCoord[ccx]; // Sym-Koordinate
    syms := sc1.sym;

    n := 0;
    while (syms and (1 shl n)) = 0 do
      Inc(n);
    altccy := UDCentCoordSymTransform[ccy, n];
    altslx := UDBrick256CoordSymTransform[slx, n];
    key := (sc1.c_idx shl 8) + altslx;

    if UDStates10Table[key].used = 0 then
      Exit;

    if findLowerIndexUDStates10(key, altccy) <> -1 then
      exit;

  end;
  { TODO : Reihenfolge untersuchen }
  if ((UDCentBrick256Prun[B_24_8 * slx + ccx] > togo) or
    (UDCentBrick256Prun[B_24_8 * sly + ccy] > togo)) then
    Exit;



  if togo = 0 then
  begin
    fc.found := True;
    for aa := 0 to maxThreads - 1 do
      tud[aa].Terminate; // alle Threads beenden
  end
  else
  begin
    if Terminated then
      Exit;

    if stopProgram = True then
    begin
      for aa := 0 to maxThreads - 1 do
        tud[aa].Terminate; // alle Threads beenden
      Exit;
    end;

    mv := InitMove;
    while True do
    begin
      if fc.mvIdx = 0 then
      begin
        mv := nextMovePhase1[NoMove, mv];
        if not (Ord(mv) >= 3 * ax) and (Ord(mv) < 3 * ax + 3) and (mv < NoMove)
        // +3: 54/18
        then
          continue;
        // nur dieser teil wird für den 1. Zug akzeptiert
      end
      else
        mv := nextMovePhase1[fc.fxymoves[fc.mvIdx - 1], mv];
      if (mv < xU1) and not UDfaceMoveAllowed[slx, Ord(mv)] then
        continue;

      if mv = NoMove then
      begin
        Exit;
      end
      else
      begin
        case mv of
          fU1..fB3:
          begin
            newccx := UDCenterMove[ccx, Ord(mv)];
            newccy := UDCenterMove[ccy, Ord(mv)];
            newslx := slx;
            newsly := sly;
          end;
          xU1..xB3:
          begin
            newccx := UDCenterMove[ccx, Ord(mv)];
            newslx := UDBrick256Move[slx, Ord(mv)];
            newccy := UDCenterMove[ccy, Ord(mv) + 18];
            newsly := UDBrick256Move[sly, Ord(mv) + 18];
          end;
          yU1..yB3:
          begin
            newccx := UDCenterMove[ccx, Ord(mv)];
            newslx := UDBrick256Move[slx, Ord(mv)];
            newccy := UDCenterMove[ccy, Ord(mv) - 18];
            newsly := UDBrick256Move[sly, Ord(mv) - 18];
          end;
        end;
         { TODO : check this }
        // dies bringt 2-3s
        //if (newccx = ccx) and (newslx = slx) and (newccy = ccy) then
        //  continue;

        fc.fxymoves[fc.mvIdx] := mv;
        Inc(fc.mvIdx);
        SearchUDCenter(newccx, newslx, newccy, newsly, togo - 1);

        if (fc.found) or Terminated then
          // kehre zurück, ohne mvIdx zu verändern
          Exit;
        Dec(fc.mvIdx);
      end;
    end;

  end;

end;

procedure makeUDAll.showstuff;
begin
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add('Phase 1 - UD Centers to UD Faces:');
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add('+cross:');
end;

procedure makeUDAll.showstuff2;
begin
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add('cluster pairs:');
end;

procedure makeUDAll.showstuff3;
begin
  Form1.Memo1.Lines.Add('computation aborted...');
  Form1.BPhase1.Caption := 'Solve Phase 1';
end;

procedure makeUDAll.showstuff4;
begin
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add('xcross:');
end;

procedure makeUDAll.showstuff5;
begin
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add('Number of moves in phase 1: ' + IntToStr(ii));
  Form1.Memo1.Lines.Add('');
  Form1.BPhase1.Caption := 'Solve Phase 1';
end;

procedure makeUDAll.showstuff6;
begin
  Form1.Memo1.Lines.Add('End Execution ' + IntToStr(Ord(ii)));
end;


procedure makeUDAll.printApply;
begin
  fcfc.printMoves(ii, jj);
  fcfc.applyMoves(ii, jj);
end;

procedure makeUDAll.Execute;
var
  i, j, totalLength: integer;
  fc: faceletcube;
  tt: threadWrapper;
begin
  //++++++++++++++++++++++ make Plus-cross of phase1 +++++++++++++++++++++++++++++++
  fc := Form1.fcube;

  totalLength := 0;
  synchronize(@showstuff);

  if Odd(fc.size) then
    for i := 1 to fc.size div 2 - 1 do
    begin
      if Form1.fcube.MakeUDPlusCross1(i, 20) then
      begin
        Inc(totalLength, fc.mvIdx);
        ii := i;
        jj := fc.size div 2;
        fcfc := fc;
        synchronize(@printApply);
      end;
    end;
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //++++++++++++++++++++++++++++ fix center orbits of phase 1 ++++++++++++++++++
  synchronize(@showstuff2);

  for i := 1 to fc.size div 2 - 2 do
    for j := i + 1 to fc.size div 2 - 1 do
    begin
      if Terminated then
        Exit;
      if stopProgram then
      begin
        synchronize(@showstuff3);
        Exit;
      end;
      tt := threadWrapper.Create(i, j, 25, fc);
      tt.WaitFor; // important!
      Inc(totalLength, fc.mvIdx);
      ii := i;
      jj := j;
      synchronize(@printApply);
    end;

  //++++++++++++++++++++++++++++ make X-cross of phase 1 +++++++++++++++++++++++
  synchronize(@showstuff4);
  for i := 1 to fc.size div 2 - 1 do
    if Form1.fcube.MakeUDXCross(i, 20) then
    begin
      Inc(totalLength, fc.mvIdx);
      ii := i;
      jj := i;
      fcfc := fc;
      synchronize(@printApply);
    end;
  Inc(grandTotal, totalLength);
  ii := totalLength;
  synchronize(@showstuff5);
end;

constructor blueprint.Create(a: Axis);
begin
  inherited Create(True); // nicht starten
  ax := a;
end;


procedure blueprint.showstuff;
begin
  Form1.Memo1.Lines.Add('End Execution ' + IntToStr(Ord(ax)));
end;


procedure blueprint.Execute;
// wichtig ist, terminated abzufragen und entsprechend zu reagieren
var
  i, j, sz: UInt64;
begin
  // inherited;
  if ax = B then
    sz := 1
  else
    sz := 50000000;

  for i := 0 to sz do
  begin
    j := Random(10);
    if Terminated then
      Exit;
  end;
  synchronize(@showstuff);
end;

end.
