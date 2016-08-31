{ JCC (Jan's Chess Componenents) - This file contains a visual chess board
  Copyright (C) 2016  Jan Dette

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit Board;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Math, Types,
  // fpvectorial, svgvectorialreader, do not work properly
  Position, MoveList, Pieces;

const
  // 47 sizes from 8x8 to 100x100
  MIN_IMAGE_SIZE = 8;
  MAX_IMAGE_SIZE = 100;
  STEP_IMAGE_SIZE = 2; // Increase sizes by 2
  COUNT_OF_IMAGE_SIZES = (MAX_IMAGE_SIZE - MIN_IMAGE_SIZE) div STEP_IMAGE_SIZE + 1;

type

  TBorderStyles = set of (bsBottom, bsLeft, bsRight, bsTop);

  { TBorder }

  TBorder = class(TPersistent)
  private
    FBackground: TColor;
    FFont: TFont;
    FOnChange: TNotifyEvent;
    FSize: integer;
    FStyle: TBorderStyles;
    procedure Changed;
    procedure FFontChange(Sender: TObject);
    procedure SetBackground(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetSize(AValue: integer);
    procedure SetStyle(AValue: TBorderStyles);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    destructor Destroy; override;
  published
    property Background: TColor read FBackground write SetBackground default clWhite;
    property Font: TFont read FFont write SetFont;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Size: integer read FSize write SetSize default 20;
    property Style: TBorderStyles read FStyle write SetStyle;
  end;

  TMovePlayedEvent = procedure(AMove: TMove) of object;
  // GUI should ask the user for the desired piece, otherwise we throw an Exception
  TPromotionEvent = procedure(var PromotionPiece: TPieceType) of object;

  { TBoard }

  TBoard = class(TCustomControl)
  private
    ClickedDown: boolean;
    ClickedFile, ClickedRank: integer;
    CurrentPos: TPoint;
    FBorder: TBorder;
    FCountOfFiles: byte;
    FCountOfRanks: byte;
    FCurrentPosition: TPosition;
    FPieceDirectory: string;
    FReversed: boolean;
    FOnMovePlayed: TMovePlayedEvent;
    FOnPromotion: TPromotionEvent;
    FWhiteSquareColor, FBlackSquareColor: TColor; // Colors for White and Black Squares
    PieceImages: array[1..COUNT_OF_IMAGE_SIZES] of array[1..12] of
    TPortableNetworkGraphic;
    ImagesAreLoaded: boolean;
    InnerBoard: TRect;
    procedure FBorderChange(Sender: TObject);
    function GetSizePerSquare: integer;
    procedure SetBlackSquareColor(AValue: TColor);
    procedure SetBorder(AValue: TBorder);
    procedure SetCurrentPosition(AValue: TPosition);
    procedure SetPieceDirectory(AValue: string);
    procedure SetReversed(AValue: boolean);
    procedure SetWhiteSquareColor(AValue: TColor);
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property CountOfFiles: byte read FCountOfFiles;
    property CountOfRanks: byte read FCountOfRanks;
    property CurrentPosition: TPosition read FCurrentPosition write SetCurrentPosition;
    destructor Destroy; override;
    // Length of an edge of a single square
    property SizePerSquare: integer read GetSizePerSquare;
  published
    property Align;
    property BlackSquareColor: TColor read FBlackSquareColor
      write SetBlackSquareColor default clBlack;
    property Border: TBorder read FBorder write SetBorder;
    // is invoked, when the user moves a piece
    property OnMovePlayed: TMovePlayedEvent read FOnMovePlayed write FOnMovePlayed;
    { Specifies the Directory where vectorial images for the pieces can be found.
      They have to be named
        [b | w][b | k | n | p | q | r].svg
      with
        w, b for white or black
      and
        p, r, n, b, k, q for resp. pawn, rook, knight, bishop, king, queen }
    property OnPromotion: TPromotionEvent read FOnPromotion write FOnPromotion;
    property PieceDirectory: string read FPieceDirectory write SetPieceDirectory;
    property Reversed: boolean read FReversed write SetReversed default False;
    property WhiteSquareColor: TColor read FWhiteSquareColor
      write SetWhiteSquareColor default clWhite;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I board_icon.lrs}
  RegisterComponents('Chess', [TBoard]);
end;

{ TBorder }

procedure TBorder.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBorder.FFontChange(Sender: TObject);
begin
  Changed;
end;

procedure TBorder.SetBackground(AValue: TColor);
begin
  if FBackground = AValue then
    Exit;
  FBackground := AValue;
  Changed;
end;

procedure TBorder.SetFont(AValue: TFont);
begin
  if FFont.IsEqual(AValue) then
    Exit;
  FFont.Assign(AValue);
  Changed;
end;

procedure TBorder.SetSize(AValue: integer);
begin
  if FSize = AValue then
    Exit;
  if AValue < 0 then
    FSize := 0
  else
    FSize := AValue;
  Changed;
end;

procedure TBorder.SetStyle(AValue: TBorderStyles);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;
  Changed;
end;

procedure TBorder.Assign(Source: TPersistent);
begin
  if Source is TBorder then
  begin
    FBackground := (Source as TBorder).Background;
    FFont.Assign((Source as TBorder).Font);
    FSize := (Source as TBorder).Size;
    FStyle := (Source as TBorder).Style;
    Changed;
  end;
end;

constructor TBorder.Create;
begin
  FBackground := clWhite;
  FFont := TFont.Create;
  FFont.Color := clBlack;
  FFont.Style := FFont.Style + [fsBold];
  FFont.OnChange := @FFontChange;
  FSize := 20;
  FStyle := [bsBottom, bsLeft, bsRight, bsTop];
end;

destructor TBorder.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

{ TBoard }

procedure TBoard.SetBlackSquareColor(AValue: TColor);
begin
  if FBlackSquareColor = AValue then
    Exit;
  FBlackSquareColor := AValue;
  Invalidate;
end;

procedure TBoard.FBorderChange(Sender: TObject);
begin
  Invalidate;
end;

function TBoard.GetSizePerSquare: integer;
begin
 (* Result := Min(InnerBoard.Bottom - InnerBoard.Top, InnerBoard.Right -
    InnerBoard.Left) div 8;     *)
  Result := Min((InnerBoard.Bottom - InnerBoard.Top) div FCountOfRanks,
    (InnerBoard.Right - InnerBoard.Left) div FCountOfFiles);
end;

procedure TBoard.SetBorder(AValue: TBorder);
begin
  if FBorder = AValue then
    Exit;
  FBorder.Assign(AValue);
  Invalidate;
end;

procedure TBoard.SetCurrentPosition(AValue: TPosition);
begin
  if FCurrentPosition = AValue then
    Exit;
  FCurrentPosition := AValue;
  FCountOfFiles := FCurrentPosition.CountOfFiles;
  FCountOfRanks := FCurrentPosition.CountOfRanks;
  Invalidate;
end;

procedure TBoard.SetPieceDirectory(AValue: string);
var
  temp: array[1..COUNT_OF_IMAGE_SIZES] of array[1..12] of TPortableNetworkGraphic;
  c, t: char;
  i, j, k, l, m: integer;
begin
  ImagesAreLoaded := False;
  if (FPieceDirectory = AValue) or not DirectoryExists(AValue) then
    Exit;
  // good, directory exists
  // let's check whether for all pieces there are the expected files
  k := MIN_IMAGE_SIZE;
  l := 1;
  while DirectoryExists(AppendPathDelim(AValue) + IntToStr(k) + 'x' + IntToStr(k)) do
  begin
    i := 1;
    for c in ['b', 'w'] do
      for t in ['b', 'k', 'n', 'q', 'p', 'r'] do
      begin
        if FileExists(AppendPathDelim(AValue) + IntToStr(k) + 'x' +
          IntToStr(k) + '/' + c + t + '.png') then
        begin
          temp[l][i] := TPortableNetworkGraphic.Create;
          temp[l][i].LoadFromFile(AppendPathDelim(AValue) + IntToStr(k) +
            'x' + IntToStr(k) + '/' + c + t + '.png');
          Inc(i);
        end
        else
        begin
          for m := 1 to l - 1 do
            for j := 1 to 12 do
              temp[m][j].Free;
          for j := 1 to i - 1 do
            temp[l][j].Free;
          Exit;
        end;
      end;
    Inc(k, STEP_IMAGE_SIZE);
    Inc(l);
  end;
  if k < MAX_IMAGE_SIZE then
    Exit;
  i := 1;
  // all images were found and can be loaded
  ImagesAreLoaded := True;
  for l := 1 to COUNT_OF_IMAGE_SIZES do
    for i := 1 to 12 do
      FreeAndNil(PieceImages[l][i]);
  PieceImages := temp;
  FPieceDirectory := AValue;
end;

procedure TBoard.SetReversed(AValue: boolean);
begin
  if FReversed = AValue then
    Exit;
  FReversed := AValue;
  Invalidate;
end;

procedure TBoard.SetWhiteSquareColor(AValue: TColor);
begin
  if FWhiteSquareColor = AValue then
    Exit;
  FWhiteSquareColor := AValue;
  Invalidate;
end;

class function TBoard.GetControlClassDefaultSize: TSize;
begin
  Result.cx := 100;
  Result.cy := 100;
end;

procedure TBoard.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    ClickedDown := True;
    ClickedFile := (X - InnerBoard.Left) div SizePerSquare;
    ClickedRank := (Y - InnerBoard.Top) div SizePerSquare;
  end;
end;

procedure TBoard.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
  if ClickedDown then
  begin
    CurrentPos := Point(X, Y);
    Invalidate;
  end;
end;

procedure TBoard.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  f, r, Start, Dest: integer;
  PromoPiece: TPieceType;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if ClickedDown then
  begin
    f := (X - InnerBoard.Left) div SizePerSquare;
    r := (Y - InnerBoard.Top) div SizePerSquare;
    if (ClickedFile in [0..7]) and (ClickedRank in [0..7]) and
      (f in [0..7]) and (r in [0..7]) then
    begin
      Start := 10 * ClickedRank + ClickedFile + 21;
      Dest := 10 * r + f + 21;
      if Reversed then
      begin
        Start := 10 * (7 - ClickedRank) + (7 - ClickedFile) + 21;
        Dest := 10 * (7 - r) + (7 - f) + 21;
      end;
      // Check for Promotion and request the user to choose a piece
      PromoPiece := ptEmpty;
      if (FCurrentPosition.Squares[Start] in [ptWPawn, ptBPawn]) and
        (Dest in Rank1 + Rank8) then
      begin
        if Assigned(FOnPromotion) then
        begin
          FOnPromotion(PromoPiece);
          if PromoPiece in [ptOff, ptEmpty, ptWPawn, ptBPawn] then
            raise Exception.Create('Invalid promotion given!');
        end;
      end;
      if Assigned(FOnMovePlayed) then
      begin
        FOnMovePlayed(TMove.Create(Start, Dest, PromoPiece));
      end;
    end;
    ClickedDown := False;
  end;
  Invalidate;
end;

procedure TBoard.Paint;
var
  c1, c2: byte;
  d, i, j, k, r, f, s, t: integer;
  TextStyle: TTextStyle;
  IsMoving: boolean;
begin
  inherited Paint;
  InnerBoard := Rect(0, 0, Width, Height);
  if bsLeft in Border.Style then
    Inc(InnerBoard.Left, Border.Size);
  if bsTop in Border.Style then
    Inc(InnerBoard.Top, Border.Size);
  if bsRight in Border.Style then
    Dec(InnerBoard.Right, Border.Size);
  if bsBottom in Border.Style then
    Dec(InnerBoard.Bottom, Border.Size);
  // Let's make it a square
  InnerBoard := Rect(InnerBoard.Left, InnerBoard.Top, InnerBoard.Left +
    FCountOfFiles * SizePerSquare, InnerBoard.Top + FCountOfRanks * SizePerSquare);
  Canvas.Brush.Color := Border.Background;
  Canvas.FillRect(0, 0, Width, Height);
  d := SizePerSquare;
  // draw the 64 squares
  for i := 0 to FCountOfFiles - 1 do
    for j := 0 to FCountOfRanks - 1 do
    begin
      if ((i + j) mod 2 = 0) then
        Canvas.Brush.Color := FWhiteSquareColor
      else
        Canvas.Brush.Color := FBlackSquareColor;
      Canvas.FillRect(InnerBoard.Left + i * d, InnerBoard.Top + j * d,
        InnerBoard.Left + (i + 1) * d, InnerBoard.Top + (j + 1) * d + 1);
    end;
  // Draw Border
  if Border.Size > 0 then
  begin
    Canvas.Brush.Color := Border.Background;
    Canvas.Font.Assign(Border.Font);
    // Centered Text
    TextStyle.Alignment := taCenter;
    TextStyle.Layout := tlCenter;
    TextStyle.SystemFont := False;
    if FReversed then
    begin
      c1 := 104;
      c2 := 49;
      s := -1;
    end
    else
    begin
      c1 := 97;
      c2 := 56;
      s := 1;
    end;
    if bsTop in Border.Style then
    begin
      for i := 0 to FCountOfFiles - 1 do
        Canvas.TextRect(Rect(InnerBoard.Left + i * d, 0, InnerBoard.Left +
          (i + 1) * d, Border.Size), 0, 0, Chr(c1 + s * i), TextStyle);
    end;
    if bsBottom in Border.Style then
    begin
      for i := 0 to FCountOfFiles - 1 do
        Canvas.TextRect(Rect(InnerBoard.Left + i * d, InnerBoard.Bottom,
          InnerBoard.Left + (i + 1) * d, InnerBoard.Bottom + Border.Size), 0,
          0, Chr(c1 + s * i), TextStyle);
    end;
    if bsLeft in Border.Style then
    begin
      for i := 0 to FCountOfRanks - 1 do
        Canvas.TextRect(Rect(0, InnerBoard.Top + i * d, Border.Size,
          InnerBoard.Top + (i + 1) * d), 0, 0, Chr(c2 - s * i), TextStyle);
    end;
    if bsRight in Border.Style then
    begin
      for i := 0 to FCountOfRanks - 1 do
        Canvas.TextRect(Rect(InnerBoard.Right, InnerBoard.Top + i *
          d, InnerBoard.Right + Border.Size, InnerBoard.Top + (i + 1) * d),
          0, 0, Chr(c2 - s * i), TextStyle);
    end;
  end;
  // in design-time we need to exit here
  if not Assigned(FCurrentPosition) then
    Exit;
  // draw the pieces
  if ImagesAreLoaded then
  begin
    if d < MIN_IMAGE_SIZE then
      k := 1
    else
    if d > MAX_IMAGE_SIZE then
      k := COUNT_OF_IMAGE_SIZES
    else
      k := (d - MIN_IMAGE_SIZE) div STEP_IMAGE_SIZE + 1;
    IsMoving := False;
    for i in ValidSquares do
    begin
      case FCurrentPosition.Squares[i] of
        ptBBishop: j := 1;
        ptBKing: j := 2;
        ptBKnight: j := 3;
        ptBPawn: j := 4;
        ptBQueen: j := 5;
        ptBRook: j := 6;
        ptWBishop: j := 7;
        ptWKing: j := 8;
        ptWKnight: j := 9;
        ptWPawn: j := 10;
        ptWQueen: j := 11;
        ptWRook: j := 12;
        else
          j := 0;
      end;
      if j > 0 then
      begin
        r := i div 10 - 2;
        f := i mod 10 - 1;
        if FReversed then
        begin
          f := 7 - f;
          r := 7 - r;
        end;
        if ClickedDown and (f = ClickedFile) and (r = ClickedRank) and
          (IsWhite(FCurrentPosition.Squares[i]) = CurrentPosition.WhitesTurn) then
        begin
          IsMoving := True;
          t := j;
        end
        else
          Canvas.CopyRect(Rect(f * d + InnerBoard.Left, r * d +
            InnerBoard.Top, (f + 1) * d + InnerBoard.Left, (r + 1) * d + InnerBoard.Top),
            PieceImages[k][j].Canvas,
            Rect(0, 0, d, d));
      end;
    end;
    if IsMoving then
      // Drawing the moving piece last ensures that it stays on top
      Canvas.CopyRect(Rect(CurrentPos.X - d div 2, CurrentPos.Y -
        d div 2, CurrentPos.X + d div 2, CurrentPos.Y + d div 2),
        PieceImages[k][t].Canvas, Rect(0, 0, d, d));
  end;
end;

constructor TBoard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ClickedDown := False;
  FBorder := TBorder.Create;
  FBorder.OnChange := @FBorderChange;
  FBlackSquareColor := clBlack;
  // FCurrentPosition := TStandardPosition.Create;
  FCountOfFiles := 8;
  FCountOfRanks := 8;
  FReversed := False;
  FWhiteSquareColor := clWhite;
  ImagesAreLoaded := False;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, 250, 250);
end;

destructor TBoard.Destroy;
var
  i, j: integer;
begin
  if ImagesAreLoaded then
    for i := 1 to COUNT_OF_IMAGE_SIZES do
      for j := 1 to 12 do
        PieceImages[i][j].Free;
  FCurrentPosition.Free;
  FBorder.Free;
  inherited Destroy;
end;

end.
