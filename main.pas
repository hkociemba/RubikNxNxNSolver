unit main;

{$Define LOADPHASE1}
{$Define LOADPHASE2}
{$Define LOADPHASE3}
{$Define LOADPHASE4}

{$Define LOADALL}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons, Spin, Menus, IniFiles, cubedefs, facecube, Math;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCoEmpty: TButton;
    BCoRandom: TButton;
    BCeClean: TButton;
    BCeEmpty: TButton;
    BCeRandom: TButton;
    BCustomize1: TButton;
    BEdClean: TButton;
    BEdEmpty: TButton;
    BEdRandom: TButton;
    BEmpty: TButton;
    BPhase2: TButton;
    BRandom: TButton;
    BClean: TButton;
    BCoClean: TButton;
    BCustomize: TButton;
    Button1: TButton;
    BPhase1: TButton;
    BPhase3: TButton;
    BPhase4: TButton;
    BSolveAll: TButton;
    CheckWide: TCheckBox;
    CheckWide1: TCheckBox;
    ColorDialog: TColorDialog;
    F1: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    LoadCube: TMenuItem;
    PageControl2: TPageControl;
    PaintBoxFaces: TPaintBox;
    R1: TButton;
    SaveCube: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SpinSize1: TSpinEdit;
    SpinSliceX: TSpinEdit;
    SpinSize: TSpinEdit;
    SpinSliceY: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    U: TButton;
    R: TButton;
    F: TButton;
    D: TButton;
    L: TButton;
    B: TButton;
    Memo1: TMemo;
    U1: TButton;
    procedure BCeCleanClick(Sender: TObject);
    procedure BCeEmptyClick(Sender: TObject);
    procedure BCeRandomClick(Sender: TObject);
    procedure BCleanClick(Sender: TObject);
    procedure BCoCleanClick(Sender: TObject);
    procedure BCoEmptyClick(Sender: TObject);
    procedure BCoRandomClick(Sender: TObject);
    procedure BCustomizeClick(Sender: TObject);
    procedure BEdCleanClick(Sender: TObject);
    procedure BEdEmptyClick(Sender: TObject);
    procedure BEdRandomClick(Sender: TObject);
    procedure BEmptyClick(Sender: TObject);
    procedure BPhase1Click(Sender: TObject);
    procedure BPhase2Click(Sender: TObject);
    procedure BPhase4Click(Sender: TObject);
    procedure BRandomClick(Sender: TObject);
    procedure BSolveAllClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BPhase3Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadCubeClick(Sender: TObject);
    procedure PaintBoxFacesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxFacesMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure PaintBoxFacesPaint(Sender: TObject);
    procedure SaveCubeClick(Sender: TObject);
    procedure SpinSizeChange(Sender: TObject);
    procedure UMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    iniFile: TIniFile;
    curCol: ColorIndex;
    tablesCreated: boolean;

  const
    off = 5; // offset des fcube im Fenster
  public
    fcube: FaceletCube; // fcube for the facelet editor

  end;

var
  Form1: TForm1;
  testcount: integer;
  curFilename: string; //name of last loaded/saved file
  stopProgram: boolean; // flag for program abortion
  grandTotal: integer; // total number of moves for all phases
  hcube: FaceletCube;//for different purposes

implementation

{$R *.lfm}
{$R cursors.res}// important for custom cursors

{ TForm1 }

uses LCLIntf (*RGB*), phase1_tables, phase2_tables, phase3_tables, phase4_tables,
  UDThreaded;

procedure TForm1.LoadCubeClick(Sender: TObject);
var
  i, j, idx, sz: integer;
  a: Axis;
  fs: TFileStream;
  buf: array of byte;
  fsN: int64;
begin
  if OpenDialog1.Execute() = True then
  begin
    fs := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    curFilename := OpenDialog1.FileName;
    fsN := fs.size;
    Setlength(buf, fsN);
    sz := Round(Sqrt(fsN div 6));
    SpinSize.Value := sz;
    fs.ReadBuffer(buf[0], fsN);
    fcube.Free;
    fcube := FaceletCube.Create(PaintBoxFaces.Canvas, sz);
    PaintBoxFaces.controlstyle := PaintBoxFaces.controlstyle - [csopaque];
    PaintBoxFaces.Invalidate;
    PaintBoxFaces.controlstyle := PaintBoxFaces.controlstyle + [csopaque];
    idx := 0;
    for a := cubedefs.U to cubedefs.B do
      for i := 0 to fcube.size - 1 do
        for j := 0 to fcube.size - 1 do
        begin
          fcube.faceCols[Ord(a), i, j] := ColorIndex(buf[idx]);
          Inc(idx);
          if idx = fsN then
          begin
            Form1.Caption := ExtractFileName(curFilename);
            fs.Free;
            Exit;
          end;
        end;
  end;
  BPhase1.Enabled := True;
  BPhase2.Enabled := False;
  BPhase3.Enabled := False;
  BPhase4.Enabled := False;
end;

procedure TForm1.SaveCubeClick(Sender: TObject);
var
  i, j, sz, idx: integer;
  a: Axis;
  c: ColorIndex;
  fs: TFileStream;
  buf: array of byte;
begin
  begin
    sz := fcube.size;
    Setlength(buf, sz * sz * 6);
    if SaveDialog1.Execute() = True then
    begin
      fs := TFileStream.Create(SaveDialog1.FileName, fmCreate);
      curFilename := SaveDialog1.FileName;
      idx := 0;
      for a := cubedefs.U to cubedefs.B do
      begin
        for i := 0 to sz - 1 do
          for j := 0 to sz - 1 do
          begin
            c := fcube.faceCols[Ord(a), i, j];
            buf[idx] := Ord(c);
            Inc(idx);
          end;
      end;
      fs.WriteBuffer(buf[0], sz * sz * 6);
      fs.Free;
      Caption := ExtractFileName(curFilename);
    end;
  end;
end;

procedure TForm1.PaintBoxFacesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  k: ColorIndex;
  c: TColor;
  px, sz, i, j: integer;
begin
  if PaintBoxFaces.Cursor = 2 then // Pipette über Auswahlfeld
  begin
    c := (Sender as TPaintBox).Canvas.Pixels[X, Y];
    for k := UCol to BCol do
      if cubedefs.Color[k] = c then
      begin
        curCol := k;
        PaintBoxFacesPaint(nil);
      end;
  end
  else
  begin
    px := fcube.pix;
    sz := fcube.size;

    if Button = mbRight then
    begin
      fcube.cv.Brush.Color := cubedefs.Color[NoCol];
      k := NoCol;
    end
    else
    begin
      fcube.cv.Brush.Color := cubedefs.Color[curCol];
      k := curCol;
    end;

    // left face click?
    if (X > off) and (X < off + 3 * sz * px) and (Y > off + 2 * px * sz) and
      (Y < off + 5 * px * sz) then
    begin // jetzt das Quadrat bestimmen
      i := (Y - off - 2 * px * sz) div (3 * px);
      j := (X - off) div (3 * px);
      fcube.drawSquare(off + 3 * j * px, off + 2 * px * sz + 3 * i * px);
      fcube.faceCols[Ord(cubedefs.L), i, j] := k;
    end;

    // front face click?
    if (X > off + 3 * px * sz) and (X < off + 6 * px * sz) and
      (Y > off + 2 * px * sz) and (Y < off + 5 * px * sz) then
    begin // jetzt das Quadrat bestimmen
      i := (Y - off - 2 * px * sz) div (3 * px);
      j := (X - off - 3 * px * sz) div (3 * px);
      fcube.drawSquare(off + 3 * px * sz + 3 * j * px,
        off + 2 * px * sz + 3 * i * px);
      fcube.faceCols[Ord(cubedefs.F), i, j] := k;
    end

    // down face click?
    else if (X > off + 3 * px * sz) and (X < off + 6 * px * sz) and
      (Y > off + 5 * px * sz) and (Y < off + 8 * px * sz) then
    begin // jetzt das Quadrat bestimmen
      i := (Y - off - 5 * px * sz) div (3 * px);
      j := (X - off - 3 * px * sz) div (3 * px);
      fcube.drawSquare(off + 3 * px * sz + 3 * j * px,
        off + 5 * px * sz + 3 * i * px);
      fcube.faceCols[Ord(cubedefs.D), i, j] := k;
    end

    // back face click?
    else if (X > off + 8 * px * sz) and (X < off + 11 * px * sz) and
      (Y > off) and (Y < off + 3 * sz * px) then
    begin // jetzt das Quadrat bestimmen
      i := (Y - off) div (3 * px);
      j := (X - off - 8 * px * sz) div (3 * px);
      fcube.drawSquare(off + 8 * px * sz + 3 * j * px, off + 3 * i * px);
      fcube.faceCols[Ord(cubedefs.B), i, j] := k;
    end

    // right face click?
    else if (X > off + 6 * px * sz) and (X < off + 8 * px * sz) and
      (Y > off + 2 * px * sz - (X - off - 6 * px * sz)) and
      (Y < off + 5 * px * sz - (X - off - 6 * px * sz)) then
    begin // jetzt das Quadrat bestimmen
      i := (Y - off - 8 * px * sz + X - off) div (3 * px);
      j := (X - off - 6 * px * sz) div (2 * px);
      fcube.drawPara2(off + 6 * px * sz + 2 * j * px, off + 2 * px *
        sz + 3 * i * px - 2 * j * px);
      fcube.faceCols[Ord(cubedefs.R), i, j] := k;
    end
    // up face click?
    else if (Y > off) and (Y < off + 2 * px * sz) and
      (X > off + 5 * px * sz - Y + off) and (X < off + 8 * px * sz - Y + off) then
    begin // jetzt das Quadrat bestimmen
      i := (Y - off) div (2 * px);
      j := (X - off - 5 * px * sz + Y - off) div (3 * px);
      fcube.drawPara1(off + 5 * px * sz - 2 * px * i + 3 * px * j,
        off + 2 * i * px);
      fcube.faceCols[Ord(cubedefs.U), i, j] := k;
    end;

  end;
end;

procedure TForm1.PaintBoxFacesMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  s, px: integer;
begin
  px := fcube.pix;
  s := fcube.getSize;
  if (X > off + 7 * px * s) and (X < off + 7 * px * s + s * px * 3 + 8) and
    (Y > off + 5 * px * s) and (Y < off + 5 * px * s + s * px * 2 + 4) then
    (Sender as TPaintBox).Cursor := 2
  else
    (Sender as TPaintBox).Cursor := 1;

end;

procedure TForm1.PaintBoxFacesPaint(Sender: TObject);
var
  s, sx, sy, px, i, j: integer;
  col: TColor;
begin
  s := fcube.getSize;
  sx := (PaintBoxFaces.Width - 2 * off) div (11 * s);
  sy := (PaintBoxFaces.Height - 2 * off) div (8 * s);
  fcube.pix := Max(1, Min(sx, sy));
  px := fcube.pix;
  if not (Sender = nil) then // sonst wird nur Farbe gewählt

    fcube.DrawCube(off, off);
  { TODO : nicht zeichnen, fallsl nur Farbe gewählt }

  for i := 0 to 2 do
    for j := 0 to 1 do
    begin
      col := cubedefs.Color[ColorIndex(i + 3 * j)];
      PaintBoxFaces.Canvas.Brush.Color := col;
      PaintBoxFaces.Canvas.Pen.Width := 5;
      PaintBoxFaces.Canvas.Pen.Color := clWhite;
      PaintBoxFaces.Canvas.Rectangle(off + 7 * px * s + s * px * i + 4 * i,
        off + 5 * px * s + s * px * j + 4 * j, off + 7 * px * s + s *
        px + s * px * i + 4 * i, off + 5 * px * s + s * px + s * px * j + 4 * j);
      PaintBoxFaces.Canvas.Pen.Color := clBlack;
      if col = cubedefs.Color[curCol] then
        PaintBoxFaces.Canvas.Pen.Width := 5
      else
      begin
        PaintBoxFaces.Canvas.Pen.Width := 1;
      end;

      PaintBoxFaces.Canvas.Rectangle(off + 7 * px * s + s * px * i + 4 * i,
        off + 5 * px * s + s * px * j + 4 * j, off + 7 * px * s + s *
        px + s * px * i + 4 * i, off + 5 * px * s + s * px + s * px * j + 4 * j);
    end;
  PaintBoxFaces.Canvas.Pen.Width := 1;
  PaintBoxFaces.Canvas.Pen.Color := clBlack;
end;



procedure TForm1.SpinSizeChange(Sender: TObject);
begin
  fcube.Free;
  fcube := FaceletCube.Create(PaintBoxFaces.Canvas, (Sender as TSpinEdit).Value);
  PaintBoxFaces.controlstyle := PaintBoxFaces.controlstyle - [csopaque];
  PaintBoxFaces.Invalidate;
  PaintBoxFaces.controlstyle := PaintBoxFaces.controlstyle + [csopaque];
  SpinSliceX.MaxValue := SpinSize.Value div 2 - 1;
  SpinSliceY.MaxValue := SpinSize.Value div 2 - 1;
  SpinSliceX.Value := 1;
  SpinSliceY.Value := 1;
  if SpinSize.Value = 1 then
  begin
    SpinSliceX.Enabled := False;
    SpinSliceY.Enabled := False;
  end
  else
  begin
    SpinSliceX.Enabled := True;
    SpinSliceY.Enabled := True;
  end;
  BPhase1.Enabled := True;
  BPhase2.Enabled := False;
  BPhase3.Enabled := False;
  BPhase4.Enabled := False;
end;




procedure TForm1.UMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  i, v: integer;
  a: Axis;
begin
  a := Axis((Sender as TButton).Tag); // Tag enthält die Drehachse
  v := SpinSliceX.Value;
  if (ssCtrl in Shift) and not (ssAlt in Shift) then
    v := SpinSliceX.Value
  else if (ssAlt in Shift) and not (ssCtrl in Shift) then
    v := SpinSliceY.Value
  else
    v := 0; // dann faceturn

  if Button = mbRight then
  begin
    if CheckWide.Checked then
      for i := 0 to v do
      begin
        fcube.move(a, i);
        fcube.move(a, i);
        fcube.move(a, i);
      end
    else
    begin
      fcube.move(a, v);
      fcube.move(a, v);
      fcube.move(a, v);
    end;
  end
  else if Button = mbLeft then
  begin
    if CheckWide.Checked then
      for i := 0 to v do
        fcube.move(a, i)
    else
      fcube.move(a, v);
  end
  else if Button = mbMiddle then
  begin
    if CheckWide.Checked then
      for i := 0 to v do
      begin
        fcube.move(a, i);
        fcube.move(a, i);
      end
    else
    begin
      fcube.move(a, v);
      fcube.move(a, v);
    end;
  end;

end;

procedure TForm1.BCustomizeClick(Sender: TObject);
begin
  ColorDialog.Color := cubedefs.Color[curCol];
  if ColorDialog.Execute = True then
  begin
    cubedefs.Color[curCol] := ColorDialog.Color;
    case curCol of
      UCol:
        iniFile.WriteInteger('Color Scheme', 'UColor', cubedefs.Color[curCol]);
      RCol:
        iniFile.WriteInteger('Color Scheme', 'RColor', cubedefs.Color[curCol]);
      FCol:
        iniFile.WriteInteger('Color Scheme', 'FColor', cubedefs.Color[curCol]);
      DCol:
        iniFile.WriteInteger('Color Scheme', 'DColor', cubedefs.Color[curCol]);
      LCol:
        iniFile.WriteInteger('Color Scheme', 'LColor', cubedefs.Color[curCol]);
      BCol:
        iniFile.WriteInteger('Color Scheme', 'BColor', cubedefs.Color[curCol]);
      NoCol:
        iniFile.WriteInteger('Color Scheme', 'FreeColor',
          cubedefs.Color[curCol]);
    end;

    PaintBoxFaces.Invalidate;
  end;
end;

procedure TForm1.BEdCleanClick(Sender: TObject);
var
  i, sz: integer;
  a: Axis;
begin
  sz := fcube.size;
  for a := cubedefs.U to cubedefs.B do
    for i := 1 to sz - 2 do
    begin
      fcube.faceCols[Ord(a), 0, i] := ColorIndex(Ord(a));
      fcube.faceCols[Ord(a), i, 0] := ColorIndex(Ord(a));
      fcube.faceCols[Ord(a), sz - 1, i] := ColorIndex(Ord(a));
      fcube.faceCols[Ord(a), i, sz - 1] := ColorIndex(Ord(a));
    end;
  PaintBoxFaces.Invalidate;
end;

procedure TForm1.BEdEmptyClick(Sender: TObject);
var
  i, sz: integer;
  a: Axis;
begin
  sz := fcube.size;
  for a := cubedefs.U to cubedefs.B do
    for i := 1 to sz - 2 do
    begin
      fcube.faceCols[Ord(a), 0, i] := NoCol;
      fcube.faceCols[Ord(a), i, 0] := NoCol;
      fcube.faceCols[Ord(a), sz - 1, i] := NoCol;
      fcube.faceCols[Ord(a), i, sz - 1] := NoCol;
    end;
  PaintBoxFaces.Invalidate;
end;

procedure TForm1.BEdRandomClick(Sender: TObject);
var
  i, slice, a, sz, max: integer;
begin
  sz := fcube.size div 2;
  max := fcube.size * 300;
  for i := 1 to max do
  begin
    a := random(6);
    slice := random(sz) + 1;
    fcube.move(Axis(a), slice);
  end;
  BCeCleanClick(nil);
  BCoCleanClick(nil);
  PaintBoxFaces.Invalidate;
end;

procedure TForm1.BEmptyClick(Sender: TObject);
var
  i, j, sz: integer;
  a: Axis;
begin
  sz := fcube.size;
  for a := cubedefs.U to cubedefs.B do
    for i := 0 to sz - 1 do
      for j := 0 to sz - 1 do
        fcube.faceCols[Ord(a), i, j] := NoCol;
  PaintBoxFaces.Invalidate;
end;

procedure TForm1.BPhase1Click(Sender: TObject);
var
  tt: makeUDAll;

begin
  if BPhase1.Caption = 'Solve Phase 1' then
  begin
    BPhase1.Caption := 'Abort';
    stopProgram := False;
    grandTotal := 0;
    tt := makeUDAll.Create(True); // do not start
    tt.FreeOnTerminate := True;
    tt.Resume;
  end
  else
  begin
    BPhase1.Caption := 'Solve Phase 1';
    stopProgram := True;
    BPhase2.Enabled := False;
  end;

end;

procedure TForm1.BPhase2Click(Sender: TObject);
var
  i, j, totalLength,ns: integer;
  av:Double;
  fc: FaceletCube;
begin
  fc := fcube;
  totalLength := 0;
  Memo1.Lines.Add('Phase 2 - RL, FB centers to RL, FB faces:');

  //++++++++++++++++++++++ fix Plus-cross of phase2 ++++++++++++++++++++++++++++
  Memo1.Lines.Add('');
  Memo1.Lines.Add('+cross:');
  ns:=0;
  for i := 1 to fc.size div 2 - 1 do
  begin
    if Form1.fcube.MakeFBPlusCross(i) then
    begin
      Inc(totalLength, fc.mvIdx);
      Inc(ns, fc.mvIdx);
      fc.printMoves(i, fc.size div 2);
      fc.applyMoves(i, fc.size div 2);
    end;
  end;
  av:= ns/(fc.size div 2 - 1);
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add(Format('+cross phase 2: %d moves, %.2f moves/orbit on average.',[ns,av]));
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //++++++++++++++++++++++++++++ fix center orbits of phase 2 ++++++++++++++++++
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add('(x,y) and (y,x) orbits:');
  ns:=0;
  for i := fc.size div 2 - 2 downto 1 do // size uneven
    for j := i + 1 to fc.size div 2 - 1 do
    begin
      if fcube.MakeFBFullCenter(i, j) then
      begin
        Inc(totalLength, fcube.mvIdx);
        Inc(ns, fc.mvIdx);
        fcube.printMoves(i, j);
        fcube.applyMoves(i, j);
      end;
    end;
    av:= ns/(fc.size div 2 - 1)/(fc.size div 2 - 2);
    Form1.Memo1.Lines.Add('');
    Form1.Memo1.Lines.Add(Format('(x,y),(y,x) orbits phase 2: %d moves, %.2f moves/orbit on average.',[ns,av]));
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //++++++++++++++++++++++++++++ fix x-cross of phase 2 ++++++++++++++++++++++++
  Form1.Memo1.Lines.Add('');
  Memo1.Lines.Add('xcross:');
  ns:=0;
  for i := 1 to fc.size div 2 - 1 do
  begin
    if fc.MakeFBXCross(i) then
    begin
      Inc(totalLength, fc.mvIdx);
      Inc(ns, fc.mvIdx);
      fc.printMoves(i, i);
      fc.applyMoves(i, i);
    end;
  end;
  av:= ns/(fc.size div 2 - 1);
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add(Format('x-cross phase 2: %d moves, %.2f moves/orbit on average.',[ns,av]));
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  Inc(grandTotal, totalLength);
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add('Number of moves in phase 2: ' + IntToStr(totalLength));
  Form1.Memo1.Lines.Add('');
  BPhase3.Enabled := True;
  Application.ProcessMessages;
end;

procedure TForm1.BPhase3Click(Sender: TObject);
var
  i, j, totalLength, ns: integer;
  av:Double;
  fc: FaceletCube;
begin
  fc := fcube;
  totalLength := 0;
  Memo1.Lines.Add('Phase 3 R,L,F and B centers to their faces:');

  //++++++++++++++++++++++ fix +-cross of phase3 ++++++++++++++++++++++++++++
  Memo1.Lines.Add('');
  Memo1.Lines.Add('+cross:');
  ns:=0;
  for i := 1 to fc.size div 2 - 1 do
  begin
    if Form1.fcube.MakePh3RLFBPlusCross(i) then
    begin
      Inc(totalLength, fc.mvIdx);
      Inc(ns, fc.mvIdx);
      fc.printMoves(i, fc.size div 2);
      fc.applyMoves(i, fc.size div 2);
    end;
  end;
  av:= ns/(fc.size div 2 - 1);
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add(Format('+cross phase 3: %d moves, %.2f moves/orbit on average.',[ns,av]));
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //++++++++++++++++++++++++++++ fix center orbits of phase 3 ++++++++++++++++++
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add('(x,y) and (y,x) orbits:');
  ns:=0;
  for i := fc.size div 2 - 2 downto 1 do  // size uneven
    for j := i + 1 to fc.size div 2 - 1 do
    begin
      if fcube.MakePh3Cent702(i, j) then
      begin
        Inc(totalLength, fcube.mvIdx);
        Inc(ns, fc.mvIdx);
        fcube.printMoves(i, j);
        fcube.applyMoves(i, j);
      end;
    end;
    av:= ns/(fc.size div 2 - 1)/(fc.size div 2 - 2);
    Form1.Memo1.Lines.Add('');
    Form1.Memo1.Lines.Add(Format('(x,y),(y,x) orbits phase 3: %d moves, %.2f moves/orbit on average.',[ns,av]));
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //++++++++++++++++++++++++++++ fix x-cross of phase 3 ++++++++++++++++++++++++
  Form1.Memo1.Lines.Add('');
  Memo1.Lines.Add('xcross:');
  ns:=0;
  for i := 1 to fc.size div 2 - 1 do
  begin
    if fc.MakePh3XCross(i) then
    begin
      Inc(totalLength, fc.mvIdx);
      Inc(ns, fc.mvIdx);
      fc.printMoves(i, i);
      fc.applyMoves(i, i);
    end;
  end;
  av:= ns/(fc.size div 2 - 1);
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add(Format('x-cross phase 3: %d moves, %.2f moves/orbit on average.',[ns,av]));
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  Inc(grandTotal, totalLength);
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add('Number of moves in phase 3: ' + IntToStr(totalLength));
  Form1.Memo1.Lines.Add('');
  Application.ProcessMessages;
  BPhase4.Enabled := True;
end;


procedure TForm1.BPhase4Click(Sender: TObject);
var
  i, j, totalLength, ns: integer;
  av:Double;
  fc: FaceletCube;
begin

  fc := fcube;
  totalLength := 0;
  Memo1.Lines.Add('Phase 4 - U and D centers to their faces:');

  //++++++++++++++++++++++ fix +-cross of phase4 +++++++++++++++++++++++++++
  Memo1.Lines.Add('');
  Memo1.Lines.Add('+cross:');
  ns:=0;
  for i := 1 to fc.size div 2 - 1 do
  begin
    if Form1.fcube.MakePh4UDPlusCross(i) then
    begin
      Inc(totalLength, fc.mvIdx);
      Inc(ns, fc.mvIdx);
      fc.printMoves(i, fc.size div 2);
      fc.applyMoves(i, fc.size div 2);
    end;
  end;
  av:= ns/(fc.size div 2 - 1);
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add(Format('+cross phase 4: %d moves, %.2f moves/orbit on average.',[ns,av]));
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //++++++++++++++++++++++++++++ fix center orbits of phase 4 ++++++++++++++++++
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add('(x,y) and (y,x) orbits:');
  ns:=0;
  for i := fc.size div 2 - 2 downto 1 do  // size uneven
    for j := i + 1 to fc.size div 2 - 1 do
    begin
      if fcube.MakePh4UDCenters(i, j) then
      begin
        Inc(totalLength, fcube.mvIdx);
        Inc(ns, fc.mvIdx);
        fcube.printMoves(i, j);
        fcube.applyMoves(i, j);
      end;
    end;
  av:= ns/(fc.size div 2 - 1)/(fc.size div 2 - 2);
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add(Format('(x,y),(y,x) orbits phase 4: %d moves, %.2f moves/orbit on average.',[ns,av]));
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //++++++++++++++++++++++++++++ fix x-cross of phase 4 ++++++++++++++++++++++++
  Form1.Memo1.Lines.Add('');
  Memo1.Lines.Add('xcross:');
  ns:=0;
  for i := 1 to fc.size div 2 - 1 do
  begin
    if fc.MakePh4XCross(i) then
    begin
      Inc(totalLength, fc.mvIdx);
      Inc(ns, fc.mvIdx);
      fc.printMoves(i, i);
      fc.applyMoves(i, i);
    end;
  end;
  av:= ns/(fc.size div 2 - 1);
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add(Format('x-cross phase 4: %d moves, %.2f moves/orbit on average.',[ns,av]));
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  Inc(grandTotal, totalLength);
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add('Number of moves in phase 4: ' + IntToStr(totalLength));
  Form1.Memo1.Lines.Add('');
  Form1.Memo1.Lines.Add('Total number of moves for solving the centers: ' + IntToStr(grandTotal));
  Form1.Memo1.Lines.Add('');
  Application.ProcessMessages;
  //for i := 1 to fc.size div 2 - 1 do //should be 0 for all edge orbits
  //  Memo1.Lines.Add(Format('%d', [fcube.edgeParity(i)]));
end;

procedure TForm1.BRandomClick(Sender: TObject);
var
  i, slice, a, sz, max: integer;
begin
  sz := fcube.size div 2;
  max := fcube.size * fcube.size * 3;
  for i := 1 to max do
  begin
    a := random(6);
    slice := random(sz);
    fcube.move(Axis(a), slice);
  end;
  BPhase2.Enabled := False;
  BPhase3.Enabled := False;
  BPhase4.Enabled := False;
end;

procedure TForm1.BSolveAllClick(Sender: TObject);
begin
end;

procedure TForm1.Button1Click(Sender: TObject);
//test routine for several purposes
var
  i, j, cls, sm, repcoord, coordtrans: integer;
  s: string;
begin
j:= SpinSliceX.Value;
fcube.getEdgeCluster(j);


  fcube.getEdgeCluster(SpinSliceX.Value);
  s := '';
  for i := 0 to 23 do
    s := s + IntToStr(fcube.ecls[i]) + ' ';
  Memo1.Lines.Add(s);


   fcube.getEdgeCluster(SpinSliceX.MaxValue+1);
  s := '';
  for i := 0 to 23 do
    s := s + IntToStr(fcube.ecls[i]) + ' ';
  Memo1.Lines.Add(s);



  PaintBoxFaces.Invalidate;
  Application.ProcessMessages;

end;



procedure TForm1.FormActivate(Sender: TObject);
//put the code here and not into create.
begin
{$IFDEF LOADPHASE1}
  createUDCentersSlice10;
  BPhase1.Enabled := True;
  ;
{$ENDIF}
 {$IFDEF LOADPHASE2}

  BPhase2.Enabled := True;
  ;
{$ENDIF}
{$IFDEF LOADPHASE3}
  createPh3Brick702RLFBCentPruningTable;
  BPhase3.Enabled := True;
{$ENDIF}
{$IFDEF LOADPHASE4}
  BPhase4.Enabled := True;
{$ENDIF}

{$IFDEF LOADALL}
  BPhase2.Enabled := False;
  BPhase3.Enabled := False;
  BPhase4.Enabled := False;
{$ENDIF}
end;

procedure TForm1.BCleanClick(Sender: TObject);
var
  i, j, sz: integer;
  a: Axis;
begin
  sz := fcube.size;
  for a := cubedefs.U to cubedefs.B do
    for i := 0 to sz - 1 do
      for j := 0 to sz - 1 do
        fcube.faceCols[Ord(a), i, j] := ColorIndex(Ord(a));
  PaintBoxFaces.Invalidate;
end;

procedure TForm1.BCoCleanClick(Sender: TObject);
var
  sz: integer;
  a: Axis;
begin
  sz := fcube.size;
  for a := cubedefs.U to cubedefs.B do
  begin
    fcube.faceCols[Ord(a), 0, 0] := ColorIndex(Ord(a));
    fcube.faceCols[Ord(a), 0, sz - 1] := ColorIndex(Ord(a));
    fcube.faceCols[Ord(a), sz - 1, 0] := ColorIndex(Ord(a));
    fcube.faceCols[Ord(a), sz - 1, sz - 1] := ColorIndex(Ord(a));
  end;
  PaintBoxFaces.Invalidate;
end;

procedure TForm1.BCoEmptyClick(Sender: TObject);
var
  sz: integer;
  a: Axis;
begin
  sz := fcube.size;
  for a := cubedefs.U to cubedefs.B do
  begin
    fcube.faceCols[Ord(a), 0, 0] := NoCol;
    fcube.faceCols[Ord(a), 0, sz - 1] := NoCol;
    fcube.faceCols[Ord(a), sz - 1, 0] := NoCol;
    fcube.faceCols[Ord(a), sz - 1, sz - 1] := NoCol;
  end;
  PaintBoxFaces.Invalidate;
end;

procedure TForm1.BCoRandomClick(Sender: TObject);
var
  i, a: integer;
begin
  for i := 1 to 1000 do
  begin
    a := random(6);
    fcube.move(Axis(a), 1);
  end;
  BCeCleanClick(nil);
  BEdCleanClick(nil);
  PaintBoxFaces.Invalidate;
end;

procedure TForm1.BCeEmptyClick(Sender: TObject);
var
  i, j, sz: integer;
  a: Axis;
begin
  sz := fcube.size;
  for a := cubedefs.U to cubedefs.B do
    for i := 1 to sz - 2 do
      for j := 1 to sz - 2 do
        fcube.faceCols[Ord(a), i, j] := NoCol;

  PaintBoxFaces.Invalidate;
end;

procedure TForm1.BCeRandomClick(Sender: TObject);
var
  i, slice, a, sz, max: integer;
begin
  sz := fcube.size div 2;
  max := fcube.size * fcube.size * 3;
  for i := 1 to max do
  begin
    a := random(6);
    slice := random(sz - 1) + 2;
    fcube.move(Axis(a), slice);
  end;
  BCoCleanClick(nil);
  BEdCleanClick(nil);
  PaintBoxFaces.Invalidate;
end;

procedure TForm1.BCeCleanClick(Sender: TObject);
var
  i, j, sz: integer;
  a: Axis;
begin
  sz := fcube.size;
  for a := cubedefs.U to cubedefs.B do
    for i := 1 to sz - 2 do
      for j := 1 to sz - 2 do
        fcube.faceCols[Ord(a), i, j] := ColorIndex(Ord(a));

  PaintBoxFaces.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  testcount := 0;
  grandTotal := -1; // -1 means unvalid

  curFilename := 'untitled.txt';
  Caption := ExtractFileName(curFilename);

  tablesCreated := False;

  curCol := UCol;
  cubedefs.Color[UCol] := clBlue;
  cubedefs.Color[RCol] := clYellow;
  cubedefs.Color[FCol] := clRed;
  cubedefs.Color[DCol] := clGreen;
  cubedefs.Color[LCol] := clWhite;
  cubedefs.Color[BCol] := RGB(255, 128, 0); // orange
  cubedefs.Color[NoCol] := clGray;

  iniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'cube.ini');

  cubedefs.Color[UCol] := iniFile.ReadInteger('Color Scheme', 'UColor',
    cubedefs.Color[UCol]);
  cubedefs.Color[RCol] := iniFile.ReadInteger('Color Scheme', 'RColor',
    cubedefs.Color[RCol]);
  cubedefs.Color[FCol] := iniFile.ReadInteger('Color Scheme', 'FColor',
    cubedefs.Color[FCol]);
  cubedefs.Color[DCol] := iniFile.ReadInteger('Color Scheme', 'DColor',
    cubedefs.Color[DCol]);
  cubedefs.Color[LCol] := iniFile.ReadInteger('Color Scheme', 'LColor',
    cubedefs.Color[LCol]);
  cubedefs.Color[BCol] := iniFile.ReadInteger('Color Scheme', 'BColor',
    cubedefs.Color[BCol]);
  cubedefs.Color[NoCol] := iniFile.ReadInteger('Color Scheme', 'FreeColor',
    cubedefs.Color[NoCol]);

  fcube := FaceletCube.Create(PaintBoxFaces.Canvas, SpinSize.Value);
  hcube := FaceletCube.Create(nil, 11);//arbitrary value
  // facelet editor
  Screen.Cursors[1] := LoadCursor(HInstance, 'Eimer'); // cursor for filling
  Screen.Cursors[2] := LoadCursor(HInstance, 'Pipette');
  PaintBoxFaces.Cursor := 1;
  // prevent flickering
  PaintBoxFaces.controlstyle := PaintBoxFaces.controlstyle + [csopaque];
  // cursor for colorpicking
  SpinSliceX.MaxValue := SpinSize.Value div 2 - 1;
  SpinSliceY.MaxValue := SpinSize.Value div 2 - 1;

{$IFDEF LOADPHASE1 OR $IFDEF LOADALL}
  BPhase1.Enabled := False;
  createNextMovePhase1Table;
  createUDBrick256CoordSymTransTable;
  createUDFaceMoveAllowedTable;

  createUDCenterCoordToSymCoordTable;
  createUDCenterCoordSymTransTable;
  createUDCenterMoveTable;
  createUDBrick256MoveTable;
  createUDXCrossMoveTable;
  createDistanceTable;

  createUDPlusCross1PruningTable;
  createUDCentXBrick256CoordPruningTable;
  createUDXCrossPruningTable;


{$ENDIF}
{$IFDEF LOADPHASE2 OR $IFDEF LOADALL}
  BPhase2.Enabled := False;
  createNextMovePhase2Table;
  createFBCenterMoveTable;
  createFBSliceMoveTable;
  createFBFaceMoveAllowedTable;
  createFBPlusCrossPruningTable;
  createFBFullCenterSliceCoordPruningTable;
  createFBXCrossMoveTable;
  createFBXCrossPruningTable;

{$ENDIF}
{$IFDEF LOADPHASE3 OR $IFDEF LOADALL}
  BPhase3.Enabled := False;
  createNextMovePhase3Table;

  createPh3RLFBCenterMoveTable; //important also for bricks
  createPh3RLFBXCrossMoveTable;

  createPh3Brick702CoordToSymCoordTable;

  createPh3RLFBCenterCoordSymTransTable;
  createPh3RLFBXCrossPruningTable;
  createPh3RLFBPlusCrossPruningTable;

  createDistanceTable;

{$ENDIF}
{$IFDEF LOADPHASE1 OR $IFDEF LOADALL}
  BPhase4.Enabled := False;
  createNextMovePhase4Table;
  createPhase4RLFBBrickMoveTable;
  createPhase4UDBrickMoveTable;
  createPh4CenterMoveTable;
  createPhase4UDXCrossMoveTable;
  createPh4UDPlusCrossPruningTable;
  createPh4UDCentBrickPruningTable;
  createPh4UDXCrossPruningTable;
{$ENDIF}
end;



end.






