unit cubedefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

const
  UDOPTIMAL = true; // langsamere aber optimale Version wenn true

type

  ColorIndex = (UCol = 0, DCol, RCol, LCol, FCol, BCol, NoCol);
  Axis = (U = 0, D, R, L, F, B);
  // f-moves sind facemoves,x-moves und y-moves sind moves der x-y-orbit slices
  Moves = (InitMove = -1, fU1, fU2, fU3, fD1, fD2, fD3, fR1, fR2, fR3, fL1, fL2,
    fL3, fF1, fF2, fF3, fB1, fB2, fB3, xU1, xU2, xU3, xD1, xD2, xD3, xR1, xR2,
    xR3, xL1, xL2, xL3, xF1, xF2, xF3, xB1, xB2, xB3, yU1, yU2, yU3, yD1, yD2,
    yD3, yR1, yR2, yR3, yL1, yL2, yL3, yF1, yF2, yF3, yB1, yB2, yB3, NoMove);

  Symmetry = (S_URF3, S_F2, S_U4, S_LR2, S_R4, S_F4); // Grundsymmetrien

  Corner = (URF = 0, UFL, ULB, UBR, DFR, DLF, DBL, DRB, NNN);
  Edge = (UR, UF, UL, UB, DR, DF, DL, DB, FR, FL, BL, BR, NN);
  CornerStatus = (CORNNOERROR, CORNPERMUTATIONERROR, CORNORIENTATIONERROR);
  EdgeStatus = (EDGENOERROR, EDGEPERMUTATIONERROR, EDGEORIENTATIONERROR,
    EDGESIZEERROR);

  OrientedCorner = record
    c: Corner;
    o: ShortInt;
  end;

  OrientedEdge = record
    e: Edge;
    o: ShortInt;
  end;

  SymCoord32 = record
    c_idx: UInt32;
    sym: UInt16;
  end;
  SymCoord16 = record
    c_idx: UInt16;
    sym: UInt16;
  end;

  States32 = record
    state: array of UInt32;
    used: Integer;
  end;

  CornerColorIndex = array [URF .. DRB, 0 .. 2] of ColorIndex;
  EdgeColorIndex = array [UR .. BR, 0 .. 1] of ColorIndex;
  EdgeClusterColorIndex = array [0 .. 23, 0 .. 1] of ColorIndex;
  CFPosition = array [URF .. DRB, 0 .. 2] of Integer;
  EFPosition = array [UR .. BR, 0 .. 1] of Integer;
  ECFPosition = array [0 .. 23, 0 .. 1] of Integer;

var
  Color: array [UCol .. NoCol] of TColor; // Global colors of Cube

const
  UDCenterID = 0; // Koordinate des Zielzustandes der UD-Center in Phase1

  B_24_8 = 735471; // Binomial(24,8)
  N_CHUNK = 45967; //Binomial(24,8) div 16 + 1
  B_16_8 = 12870; // Binomial(16,8)
  B_8_4 = 70; // Binomial(8,4)
  B_24_12 = 2704156;
  N_SYMCENTCOORD = 92247; // Anzahl der Äquivalenzklassen der UDCenterCoord
  N_SYMBRICKCOORD = 46935; // Anzahl der Äquivalenzklassen der UDBrickCoord
  N_SYMBRICK4096COORD = 640; // Number of equivalence classes of Phase3Brick4096Coord
  N_SYMBRICK702COORD =690; //Number of equivalence classes of Ph3Brick702Coord

  // wohin facelets bei  180 Drehungen, deren slice durch U,R,F gehen, abgebildet werden.
  oppositeU: array [0 .. 23] of Integer = (-1, -1, -1, -1, -1, -1, -1, -1, 14,
    15, 12, 13, -1, -1, -1, -1, 22, 23, 20, 21, -1, -1, -1, -1);
  oppositeR: array [0 .. 23] of Integer = (6, 7, 4, 5, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 20, 21, 22, 23, -1, -1, -1, -1);
  oppositeF: array [0 .. 23] of Integer = (4, 5, 6, 7, -1, -1, -1, -1, 12, 13,
    14, 15, -1, -1, -1, -1, -1, -1, -1, 1, -1, -1, -1, -1);

  // wohin facelets bei  180 Drehungen, deren slice durch U,R,F gehen, abgebildet werden.
  opposite: array [0 .. 2, 0 .. 23] of Integer = ((-1, -1, -1, -1, -1, -1, -1,
    -1, 14, 15, 12, 13, -1, -1, -1, -1, 22, 23, 20, 21, -1, -1, -1, -1),
    (6, 7, 4, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 20, 21, 22, 23,
    -1, -1, -1, -1), (4, 5, 6, 7, -1, -1, -1, -1, 12, 13, 14, 15, -1, -1, -1,
    -1, -1, -1, -1, 1, -1, -1, -1, -1));

  Phase2Allowed: Array [-1 .. 54] of Boolean = (true { initMove } , false,
    false, false, false, false, false, { fU1..fD3 }
    true, true, true, true, true, true, { fR1..fL3 }
    true, true, true, true, true, true, { fF1..fB3 }
    true, true, true, true, true, true, { xU1..xD1 }
    false, true, false, false, true, false, { xR1..xL3 }
    false, true, false, false, true, false, { xF1..xB3 }
    true, true, true, true, true, true, { yU1..yD1 }
    false, true, false, false, true, false, { yR1..yL3 }
    false, true, false, false, true, false, { yF1..yB3 }
    true { noMove }
    );

  //// gibt die gültigen Züge an, wenn in Richtung der R und F Achse zusammen
  //// gesucht wird. Wird im Augenblick noch nicht benutzt
  //Phase3FRAllowed: Array [-1 .. 54] of Boolean = (true { initMove } , false,
  //  false, false, false, false, false, { fU1..fD3 }
  //  true, true, true, true, true, true, { fR1..fL3 }
  //  true, true, true, true, true, true, { fF1..fB3 }
  //  false, true, false, false, true, false, { xU1..xD1 }
  //  false, true, false, false, true, false, { xR1..xL3 }
  //  false, true, false, false, true, false, { xF1..xB3 }
  //  false, true, false, false, true, false, { yU1..yD1 }
  //  false, true, false, false, true, false, { yR1..yL3 }
  //  false, true, false, false, true, false, { yF1..yB3 }
  //  true { noMove }
  //  );

  // In phase 3, square moves of slices or any face turns are allowed
  Phase3Allowed: Array [-1 .. 54] of Boolean = (true { initMove } , true,
    true, true, true, true, true, { fU1..fD3 }
    true, true, true, true, true, true, { fR1..fL3 }
    true, true, true, true, true, true, { fF1..fB3 }
    false, true, false, false, true, false, { xU1..xD1 }
    false, true, false, false, true, false, { xR1..xL3 }
    false, true, false, false, true, false, { xF1..xB3 }
    false, true, false, false, true, false, { yU1..yD1 }
    false, true, false, false, true, false, { yR1..yL3 }
    false, true, false, false, true, false, { yF1..yB3 }
    true { noMove }
    );

  //// Gilt, wenn in die U,R,F Richtung einzeln gesucht wird
  //Phase3Allowed: Array [0 .. 2, -1 .. 54] of Boolean =
  //// axis=U
  //  ((true { initMove } , true, true, true, true, true, true, { fU1..fD3 }
  //  false, false, false, false, false, false, { fR1..fL3 }
  //  false, false, false, false, false, false, { fF1..fB3 }
  //  false, false, false, false, false, false, { xU1..xD1 }
  //  false, true, false, false, true, false, { xR1..xL3 }
  //  false, true, false, false, true, false, { xF1..xB3 }
  //  false, false, false, false, false, false, { yU1..yD1 }
  //  false, true, false, false, true, false, { yR1..yL3 }
  //  false, true, false, false, true, false, { yF1..yB3 }
  //  true { noMove }
  //  ),
  //  // axis=R
  //  (true { initMove } , false, false, false, false, false, false,
  //  { fU1..fD3 }
  //  true, true, true, true, true, true, { fR1..fL3 }
  //  false, false, false, false, false, false, { fF1..fB3 }
  //  false, true, false, false, true, false, { xU1..xD1 }
  //  false, false, false, false, false, false, { xR1..xL3 }
  //  false, true, false, false, true, false, { xF1..xB3 }
  //  false, true, false, false, true, false, { yU1..yD1 }
  //  false, false, false, false, false, false, { yR1..yL3 }
  //  false, true, false, false, true, false, { yF1..yB3 }
  //  true { noMove }
  //  ),
  //  // axis=F
  //  (true { initMove } , false, false, false, false, false, false,
  //  { fU1..fD3 }
  //  false, false, false, false, false, false, { fR1..fL3 }
  //  true, true, true, true, true, true, { fF1..fB3 }
  //  false, true, false, false, true, false, { xU1..xD1 }
  //  false, true, false, false, true, false, { xR1..xL3 }
  //  false, false, false, false, false, false, { xF1..xB3 }
  //  false, true, false, false, true, false, { yU1..yD1 }
  //  false, true, false, false, true, false, { yR1..yL3 }
  //  false, false, false, false, false, false, { yF1..yB3 }
  //  true { noMove }
  //  ));

  // +++++++++++++the colors of the corner cubies++++++++++++++++++++++++++++++++++
  CCI: CornerColorIndex = ((UCol, RCol, FCol), (UCol, FCol, LCol),
    (UCol, LCol, BCol), (UCol, BCol, RCol), (DCol, FCol, RCol),
    (DCol, LCol, FCol), (DCol, BCol, LCol), (DCol, RCol, BCol));

  // +++++++++++++++++++++++the colors of the middle edge cubies++++++++++++++++++++++++++
  ECI: EdgeColorIndex = ((UCol, RCol), (UCol, FCol), (UCol, LCol), (UCol, BCol),
    (DCol, RCol), (DCol, FCol), (DCol, LCol), (DCol, BCol), (FCol, RCol),
    (FCol, LCol), (BCol, LCol), (BCol, RCol));

  // +++++++++++++++++++++++the colors of the 24 C(0,y) cluster cubies++++++++++++++++++
  ECCI: EdgeClusterColorIndex = ((UCol, BCol), (UCol, RCol), (UCol, FCol),
    (UCol, LCol), (DCol, FCol), (DCol, RCol), (DCol, BCol), (DCol, LCol),
    (RCol, UCol), (RCol, BCol), (RCol, DCol), (RCol, FCol), (LCol, UCol),
    (LCol, FCol), (LCol, DCol), (LCol, BCol), (FCol, UCol), (FCol, RCol),
    (FCol, DCol), (FCol, LCol), (BCol, UCol), (BCol, LCol), (BCol, DCol),
    (BCol, RCol));

  // ++++the involved facelets of the corner cubies+++++++++++++++++++++++
  // 0 means facelet is in row 0, 1 means it is in row size-1
  CFRow: CFPosition = ((1, 0, 0), (1, 0, 0), (0, 0, 0), (0, 0, 0), (0, 1, 1),
    (0, 1, 1), (1, 1, 1), (1, 1, 1));
  // the same for the columns
  CFCol: CFPosition = ((1, 0, 1), (0, 0, 1), (0, 0, 1), (1, 0, 1), (1, 1, 0),
    (0, 1, 0), (0, 1, 0), (1, 1, 0));

  // ++++the involved facelets of the middle edge cubies+++++++++++++++++++++++
  // size is supposed to be odd
  // 0 means row/col 0, 1 means row/col (size-1)/2, 2 means row/col size-1
  EFRow: EFPosition = ((1, 0), (2, 0), (1, 0), (0, 0), (1, 2), (0, 2), (1, 2),
    (2, 2), (1, 1), (1, 1), (1, 1), (1, 1));
  EFCol: EFPosition = ((2, 1), (1, 1), (0, 1), (1, 1), (2, 1), (1, 1), (0, 1),
    (1, 1), (2, 0), (0, 2), (2, 0), (0, 2));

  // ++++the involved facelets of the edge C(0,y) cluster cubies+++++++++++++++++++++++
  // 0 means row/col 0, 1 means row/col y, 2 means row/col size-1,
  // 3 means row/col size-1-y
  ECFRow: ECFPosition = ((0, 0), (1, 0), (2, 0), (3, 0), (0, 2), (1, 2), (2, 2),
    (3, 2), (0, 3), (1, 1), (2, 3), (3, 3), (0, 1), (1, 1), (2, 1), (3, 3),
    (0, 2), (1, 1), (2, 0), (3, 3), (0, 0), (1, 1), (2, 2), (3, 3));
  ECFCol: ECFPosition = ((1, 3), (2, 3), (3, 3), (0, 3), (1, 1), (2, 1), (3, 1),
    (0, 1), (1, 2), (2, 0), (3, 2), (0, 2), (1, 0), (2, 0), (3, 0), (0, 2),
    (1, 1), (2, 0), (3, 3), (0, 2), (1, 3), (2, 0), (3, 1), (0, 2));

  // für das EdgeCluster C(0,y) wird die Nummerierung so abgeändert, dass 0..11
  // die Clusterelement  unter yU2,yD2,yR2,yL2,yF2,YB2,U,D,R,L,F2,B2 sind
  remapEdges: array [0 .. 23] of Integer = (0, 1, 2, 3, 4, 5, 6, 7, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 8, 21, 9, 22, 10, 23, 11);
  invRemapEdges: array [0 .. 23] of Integer = (0, 1, 2, 3, 4, 5, 6, 7, 17, 19,
    21, 23, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 20, 22);
  // Refers to type Moves above
  MoveStrings: array [InitMove .. NoMove] of String = ('InitMove', 'fU1', 'fU2',
    'fU3', 'fD1', 'fD2', 'fD3', 'fR1', 'fR2', 'fR3', 'fL1', 'fL2', 'fL3', 'fF1',
    'fF2', 'fF3', 'fB1', 'fB2', 'fB3', 'xU1', 'xU2', 'xU3', 'xD1', 'xD2', 'xD3',
    'xR1', 'xR2', 'xR3', 'xL1', 'xL2', 'xL3', 'xF1', 'xF2', 'xF3', 'xB1', 'xB2',
    'xB3', 'yU1', 'yU2', 'yU3', 'yD1', 'yD2', 'yD3', 'yR1', 'yR2', 'yR3', 'yL1',
    'yL2', 'yL3', 'yF1', 'yF2', 'yF3', 'yB1', 'yB2', 'yB3', 'NoMove');
  ColorIndexStrings: array [UCol .. NoCol] of String = ('U', 'D', 'R', 'L', 'F',
    'B', '-');

implementation

end.

