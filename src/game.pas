{ JCC (Jan's Chess Componenents) - This file contains
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

unit Game;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MoveList, Position, Ply, fgl;

type
  {$PACKENUM 1}
  TGameResult = (grNone, grWhiteWins, grBlackWins, grDraw);
  // TODO: These have to be moved somewhere else
  {$PACKENUM 1}
  TFIDETitle = (ftNone, ftCM, ftFM, ftIM, ftGM, ftWCM, ftWFM, ftWIM, ftWGM);
  {$PACKENUM 1}
  TPlayerType = (ptNone, ptHuman, ptProgram);
  // The following two needs to be extended
  {$PACKENUM 1}
  TTerminationType = (ttNone, ttNormal, ttAbandoned, ttAdjucation, ttDeath, ttEmergency,
    ttRulesInfraction, ttTimeForfeit, ttUnterminated);
  {$PACKENUM 1}
  TModeType = (mtNone, mtOverTheBoard, mtPaperMail, mtElectronicMail, mtICS,
    mtGeneralTelecommunication);
  // The following styles are used for representation of sublines
  TRAVStyle = (rsPGN, rsChessbase, rsSCID, rsJose, rsChessX);

  TTextStyle = record
    ColorIndex: integer; // the index in the color table
    Bold: boolean;
    Italic: boolean;
  end;

  TRGBColor = record
    Red: byte;
    Green: byte;
    Blue: byte;
  end;

  TSubLineStyle = record
    Newline: boolean;      // Begin comment in next line
    BeginChar: char;       // first character in sub line e.g. (, [
    EndChar: char;         // last character in sub line e.g. ), ]
    Indentation: integer;  // number of spaces used for indentation
    // should a space char be used in front/behind End/Begin-Char
    SpaceInBetween: boolean;
  end;

  TCommentStyle = record
    // If a comment is too long to fit in current line, then start it in the next one
    LongCommentInNewline: boolean;
    // only used when line is too short. Number of spaces for indentation of comment
    Indentation: integer;
    BeginChar: char; // a special char to start the comment e.g. {
    EndChar: char;   // a special char to stop the comment e.g. }
  end;

  TNotationStyle = record
    PieceLetters: TChessPieceLetters;   // the piece letters that should be used
    ShowPawnLetter: boolean;            // show the letter on pawn moves e.g. Pe4
    ShowEnPassantSuffix: boolean;       // show e.p. if a pawn is taken en passant
    CaptureSymbol: TCaptureSymbol;      // how to write a capture e.g. exd5, ed5
    PromotionSymbol: TPromotionSymbol;  // how to write a promotion e.g. e8Q, e8=Q
    ShowNAGNumber: boolean;    // display the raw number or convert it to its meaning
    MaxLineLength: integer;             // the max length of the generated notation text
    CommentStyle: TCommentStyle;        // the format for commentary
    SubLineStyle: TSubLineStyle;        // the used format for sub lines
    SecondarySubLineStyle: TSubLineStyle; // when there is at least one more sub line
    FormatRTF: boolean;                 // Should the output be in rich text format
    // The following are only used if FormatRTF is true

    ColorTable: array[1..8] of TRGBColor;  // color table for the following text styles
    MainLineMoveStyle: TTextStyle;      // normal text style for moves in main line
    MainLineNumberStyle: TTextStyle;   // normal text style for move numbers in main line
    SecondaryMainLineMoveStyle: TTextStyle; // is used when there are also sub lines
    SecondaryMainLineNumberStyle: TTextStyle; // is used when there are also sub lines
    SubLineMoveStyle: TTextStyle;       // text style for moves in the sub lines
    SubLineNumberStyle: TTextStyle;     // text style for move numbers the sub lines
    CommentTextStyle: TTextStyle;           // text style for comments
    NAGStyle: TTextStyle;               // text style for NAG
  end;

  TTokenKind = (tkMove, tkNumber, tkBeginLine, tkEndLine, tkComment, tkNAG, tkResult);

  TToken = record
    Kind: TTokenKind;
    Value: string;
  end;

  PToken = ^TToken;

  TGameNotation = specialize TFPGList<PToken>;

  { TGameNotationHelper }

  TGameNotationHelper = class helper for TGameNotation
    procedure Add(ATokenKind: TTokenKind; AValue: string); overload;
  end;

const
  DefaultNotationStyle: TNotationStyle = (
    PieceLetters: ('P', 'N', 'B', 'R', 'Q', 'K', 'P', 'N', 'B', 'R', 'Q', 'K');
    ShowPawnLetter: False;
    ShowEnPassantSuffix: False;
    CaptureSymbol: csx;
    PromotionSymbol: psNone;
    ShowNAGNumber: False;
    MaxLineLength: 0);

type

  { TGame }

  TGame = class
  private
    FNotation: TGameNotation;
    function GetCurrentPlyNumber: word;
    procedure SetInitialPosition(AValue: TPosition);
  protected
    //  FBoard: TBoard;
    FCurrentPlyNode: TPlyTreeNode;
    FCurrentPosition: TPosition;
    FInitialPosition: TPosition;
    FPlyTree: TPlyTree;
    procedure UpdateNotation; virtual; abstract;
  public
    // Simply adds a move at current position
    procedure AddMove(AMove: TMove);
    // AMove will become part of main line and the current main line will
    // be kept as side line
    procedure AddMoveAsNewMainLine(AMove: TMove);
    // AMove will be the start of a new side line
    procedure AddMoveAsSideLine(AMove: TMove);
    constructor Create; virtual;
    constructor Create(const AInitialPosition: TPosition); virtual; abstract;
    procedure Clear;
    destructor Destroy; override;
    // This returns the last tree node of in the main line of CurrentPlyNode
    function GetLastPlyNodeInCurrentVariation: TPlyTreeNode;
    // Setups position before last move on board
    procedure GoOneMoveBackward;
    // Setups position after next move on board
    procedure GoOneMoveForward;
    // Setups position after the given tree node
    procedure GoToPositionAfterPlyNode(const APlyTreeNode: TPlyTreeNode);
    // Similiar to AddMoveAsNewMainLine but current main line will be deleted
    procedure ReplaceMainLine(AMove: TMove);
    procedure SetCommentAfterCurrentPly(const AComment: string);
    procedure SetCommentBeforeCurrentPly(const AComment: string);
  public
    property CurrentPlyNumber: word read GetCurrentPlyNumber;
    property CurrentPlyNode: TPlyTreeNode read FCurrentPlyNode;
    property CurrentPosition: TPosition read FCurrentPosition;
    property InitialPosition: TPosition read FInitialPosition write SetInitialPosition;
    property Notation: TGameNotation read FNotation;
    // NOTE: PlyTree.Root.Data will always be nil
    property PlyTree: TPlyTree read FPlyTree;
  end;

  { TStandardGame }

  TStandardGame = class(TGame)
  private
    procedure SetGameResult(AValue: TGameResult);
  protected
    FGameResult: TGameResult;
    procedure UpdateNotation; override;
  public
    constructor Create; override;
    constructor Create(const AInitialPosition: TPosition); override;
    property GameResult: TGameResult read FGameResult write SetGameResult;
  end;

function GameResultToStr(const AGameResult: TGameResult): string;
function StrToFIDETitle(const s: string): TFIDETitle;
function StrToPlayerType(const s: string): TPlayerType;
function StrToModeType(const s: string): TModeType;
function StrToTerminationType(const s: string): TTerminationType;

implementation

procedure DeletePly(const AData: TPly);
begin
  AData.Free;
end;

function GameResultToStr(const AGameResult: TGameResult): string;
begin
  case AGameResult of
    grNone: Result := '*';
    grWhiteWins: Result := '1-0';
    grBlackWins: Result := '0-1';
    grDraw: Result := '½-½';
  end;
end;

function StrToFIDETitle(const s: string): TFIDETitle;
begin
  case s of
    'CM': Result := ftCM;
    'FM': Result := ftFM;
    'IM': Result := ftIM;
    'GM': Result := ftGM;
    'WCM': Result := ftWCM;
    'WFM': Result := ftWFM;
    'WIM': Result := ftWIM;
    'WGM': Result := ftWGM;
    else
      Result := ftNone;
  end;
end;

function StrToPlayerType(const s: string): TPlayerType;
begin
  case LowerCase(s) of
    'human': Result := ptHuman;
    'program': Result := ptProgram;
    else
      Result := ptNone;
  end;
end;

function StrToModeType(const s: string): TModeType;
begin
  case LowerCase(s) of
    'otb': Result := mtNone;
    'pm': Result := mtPaperMail;
    'em': Result := mtElectronicMail;
    'ics': Result := mtICS;
    'tc': Result := mtGeneralTelecommunication;
    else
      Result := mtNone;
  end;
end;

function StrToTerminationType(const s: string): TTerminationType;
begin
  case Lowercase(s) of
    'abandoned': Result := ttAbandoned;
    'adjudication': Result := ttAdjucation;
    'death': Result := ttDeath;
    'emergency': Result := ttEmergency;
    'normal': Result := ttNormal;
    'rules infraction': Result := ttRulesInfraction;
    'time forfeit': Result := ttTimeForfeit;
    'unterminated': Result := ttUnterminated;
    else
      Result := ttNone;
  end;
end;

function StartRTF(const ATextStyle: TTextStyle): string;
begin
  Result := '';
  if ATextStyle.Bold then
    Result := Result + '\b';
  if ATextStyle.Italic then
    Result := Result + '\i';
  Result := Result + '\cf' + IntToStr(ATextStyle.ColorIndex);
end;

{ TGameNotationHelper }

procedure TGameNotationHelper.Add(ATokenKind: TTokenKind; AValue: string);
var
  Token: PToken;
begin
  New(Token);
  Token^.Kind := ATokenKind;
  Token^.Value := AValue;
  Self.Add(Token);
end;

{ TGame }

function TGame.GetCurrentPlyNumber: word;
var
  Temp: TPlyTreeNodeList;
begin
  Temp := FPlyTree.GetPathTo(FCurrentPlyNode);
  Result := Temp.Size - 1;
  Temp.Free;
end;

procedure TGame.SetInitialPosition(AValue: TPosition);
begin
  if FInitialPosition = AValue then
    Exit;
  if FPlyTree.Count > 0 then
    raise Exception.Create('Game already began, cannot change initial position');
  FInitialPosition := AValue;
end;

procedure TGame.AddMove(AMove: TMove);
begin
  FCurrentPlyNode.Children.PushBack(TPlyTreeNode.Create(TPly.Create(AMove)));
  FCurrentPlyNode := FCurrentPlyNode.Children.Items[0];
  FCurrentPosition.PlayMove(AMove);
  UpdateNotation;
end;

procedure TGame.AddMoveAsNewMainLine(AMove: TMove);
begin
  FCurrentPlyNode.Children.Insert(0, TPlyTreeNode.Create(TPly.Create(AMove)));
  FCurrentPlyNode := FCurrentPlyNode.Children.Items[0];
  FCurrentPosition.PlayMove(AMove);
  UpdateNotation;
end;

procedure TGame.AddMoveAsSideLine(AMove: TMove);
begin
  FCurrentPlyNode.Children.PushBack(TPlyTreeNode.Create(TPly.Create(AMove)));
  FCurrentPlyNode := FCurrentPlyNode.Children.Back;
  FCurrentPosition.PlayMove(AMove);
  UpdateNotation;
end;

constructor TGame.Create;
begin
  FPlyTree := TPlyTree.Create;
  FPlyTree.Root := TPlyTreeNode.Create(nil);
  FCurrentPlyNode := FPlyTree.Root;
  FNotation := TGameNotation.Create;
end;

procedure TGame.Clear;
begin
  // We need to delete all plies currently stored in the tree
  FPlyTree.DepthFirstTraverse(@DeletePly);
  FPlyTree.Root.Free;
  FPlyTree.Root := TPlyTreeNode.Create(nil);
  FCurrentPlyNode := FPlyTree.Root;
  FCurrentPosition.SetupInitialPosition;
  FInitialPosition.SetupInitialPosition;
  UpdateNotation;
end;

destructor TGame.Destroy;
var
  Token: PToken;
begin
  // We need to delete all plies currently stored in the tree
  FPlyTree.DepthFirstTraverse(@DeletePly);
  for Token in FNotation do
    Dispose(Token);
  FNotation.Free;
  FPlyTree.Free;
  FInitialPosition.Free;
  FCurrentPosition.Free;
  inherited Destroy;
end;

function TGame.GetLastPlyNodeInCurrentVariation: TPlyTreeNode;
begin
  Result := FCurrentPlyNode;
  while Result.Children.Size > 0 do
  begin
    Result := Result.Children.Items[0];
  end;
end;

procedure TGame.GoOneMoveBackward;
begin
  GoToPositionAfterPlyNode(FPlyTree.GetParentOf(FCurrentPlyNode));
end;

procedure TGame.GoOneMoveForward;
begin
  GoToPositionAfterPlyNode(FCurrentPlyNode.Children.Items[0]);
end;

procedure TGame.GoToPositionAfterPlyNode(const APlyTreeNode: TPlyTreeNode);
var
  Temp: TPlyTreeNodeList;
  Node: TPlyTreeNode;
begin
  Temp := FPlyTree.GetPathTo(APlyTreeNode);
  if Temp = nil then
    raise Exception.Create('The given node is not in the tree');
  FCurrentPosition.Copy(FInitialPosition);
  for Node in Temp do
  begin
    if Node.Data <> nil then
      FCurrentPosition.PlayMove(Node.Data.Move);
  end;
  temp.Free;
  FCurrentPlyNode := APlyTreeNode;
end;

procedure TGame.ReplaceMainLine(AMove: TMove);
begin
  FCurrentPlyNode.Children.Items[0].Free;
  FCurrentPlyNode.Children.Erase(0);
  FCurrentPlyNode.Children.Insert(0, TPlyTreeNode.Create(TPly.Create(AMove)));
  FCurrentPlyNode := CurrentPlyNode.Children.Items[0];
  FCurrentPosition.PlayMove(AMove);
end;

procedure TGame.SetCommentAfterCurrentPly(const AComment: string);
begin
  FCurrentPlyNode.Data.CommentTextInBehind := AComment;
end;

procedure TGame.SetCommentBeforeCurrentPly(const AComment: string);
begin
  FCurrentPlyNode.Data.CommentTextInFront := AComment;
end;

{ TStandardGame }

constructor TStandardGame.Create;
begin
  inherited Create;
  FInitialPosition := TStandardPosition.Create;
  FCurrentPosition := TStandardPosition.Create;
  FGameResult := grNone;
end;

constructor TStandardGame.Create(const AInitialPosition: TPosition);
begin
  Create;
  FInitialPosition.Copy(AInitialPosition);
  FCurrentPosition.Copy(AInitialPosition);
end;

procedure TStandardGame.SetGameResult(AValue: TGameResult);
begin
  if FGameResult = AValue then
    Exit;
  FGameResult := AValue;
  UpdateNotation;
end;

procedure TStandardGame.UpdateNotation;
var
  VarLevel: word;
  NeedsMoveNumber: boolean;

  procedure RecursiveLineToString(CurrentRoot: TPlyTreeNode;
    StartPos: TStandardPosition);
  var
    TempPos: TStandardPosition;

    //procedure AddNewLine(var Notation: string);
    //begin
    //  // Add a new line and delete the last char if it's a space
    //  if Notation[Length(Notation)] = ' ' then
    //    Delete(Notation, Length(Notation), 1);
    //  Notation := Notation + LineEnding;
    //  LineLength := 0;
    //end;

    //procedure AddString(var Notation: string; const s: string);
    //begin
    //  if (ANotationStyle.MaxLineLength > 0) and (LineLength +
    //    Length(s) > ANotationStyle.MaxLineLength) then
    //    AddNewLine(Notation);
    //  Notation := Notation + s;
    //  LineLength := Length(s);
    //end;

    //procedure AddComment(var Notation: string; const AComment: string);
    //var
    //  s, NextWord: string;
    //  p, Min: integer;
    //begin
    //  s := AComment;
    //  if ANotationStyle.FormatRTF then
    //    Notation := Notation + StartRTF(ANotationStyle.CommentTextStyle);
    //  if (ANotationStyle.MaxLineLength > 0) and
    //    (LineLength + Length(s) > ANotationStyle.MaxLineLength) then
    //  begin
    //    // Comment too long
    //    // calculate the the minimal start position
    //    Min := (VarLevel + 1) * ANotationStyle.CommentStyle.Indentation;
    //    if ANotationStyle.CommentStyle.LongCommentInNewline then
    //    begin
    //      // begin new line
    //      AddNewLine(Notation);
    //      Notation := Notation + StringOfChar(' ', Min);
    //      LineLength := Min;
    //    end;
    //    AddString(Notation, ANotationStyle.CommentStyle.BeginChar);
    //    while Length(s) > 0 do
    //    begin
    //      p := Pos(' ', s);
    //      if p > 0 then
    //      begin
    //        NextWord := Copy(s, 1, p);
    //      end
    //      else
    //        NextWord := s;
    //      if (LineLength = Min) and (Length(NextWord) + Min >
    //        ANotationStyle.MaxLineLength) then
    //        // word is too long
    //        NextWord := Copy(s, 1, ANotationStyle.MaxLineLength - Min);
    //      if Length(NextWord) + LineLength <= ANotationStyle.MaxLineLength then
    //      begin
    //        // Add next word to notation
    //        Notation := Notation + NextWord;
    //        Delete(s, 1, Length(NextWord));
    //      end
    //      else
    //      begin
    //        // begin new line
    //        AddNewLine(Notation);
    //        Notation := Notation + StringOfChar(' ', Min);
    //        LineLength := Min;
    //      end;
    //    end;
    //    AddString(Notation, ANotationStyle.CommentStyle.EndChar);
    //  if LineLength < ANotationStyle.MaxLineLength then
    //    Notation := Notation + ' '
    //  else
    //  end
    //  else
    //    Notation := Notation + ANotationStyle.CommentStyle.BeginChar +
    //      AComment + ANotationStyle.CommentStyle.EndChar + ' ';
    //end;

    //    procedure AddNAG(var Notation: string; const ANAG: TNAG);
    //    var
    //      NextWord: string;
    //    begin
    //      if ANotationStyle.FormatRTF then
    //        Notation := Notation + ANotationStyle.NAGStyle;
    //      if ANotationStyle.ShowNAGNumber then
    //        NextWord := '$' + IntToStr(ANAG)
    //      else
    //        NextWord := NAGToStr(ANAG);
    //      Notation := Notation + ' ';
    //    end;

    //function PlyToStr(APly: TPly; FirstMoveInVariation: boolean): string;
    //begin
    //  Result := '';
    //  // in front comment
    //  if Length(APly.CommentTextInFront) > 0 then
    //  begin
    //    AddComment(Result, APly.CommentTextInFront);
    //  end;
    //  if APly.NonStandardGlyph > 0 then
    //    Result := Result + NAGToStr(APly.NonStandardGlyph,
    //      ANotationStyle.ShowNAGNumber) + ' ';
    //  if TempPos.WhitesTurn then
    //    Result := Result + IntToStr(TempPos.MoveNumber) + '.'
    //  else
    //  if FirstMoveInVariation then
    //    Result := Result + IntToStr(TempPos.MoveNumber) + '...';
    //  Result := Result + TempPos.MoveToSAN(APly.Move, ANotationStyle.PieceLetters,
    //    ANotationStyle.ShowPawnLetter, ANotationStyle.ShowEnPassantSuffix,
    //    ANotationStyle.CaptureSymbol, ANotationStyle.PromotionSymbol);
    //  if APly.MoveAssessment > 0 then
    //    Result := Result + NAGToStr(APly.MoveAssessment,
    //      ANotationStyle.ShowNAGNumber) + ' ';
    //  if APly.PositionalAssessment > 0 then
    //    Result := Result + NAGToStr(APly.PositionalAssessment,
    //      ANotationStyle.ShowNAGNumber) + ' ';
    //  if Result[Length(Result)] <> ' ' then
    //    Result := Result + ' ';
    //  if Length(APly.CommentTextInBehind) > 0 then
    //    Result := Result + APly.CommentTextInBehind + ' ';
    //end;

    { TODO : Zugnummern }

    procedure AddPlyToNotation(const APly: TPly);
    begin
      if Length(APly.CommentTextInFront) > 0 then
        FNotation.Add(tkComment, APly.CommentTextInFront);
      if NeedsMoveNumber or TempPos.WhitesTurn then
      begin
        if TempPos.WhitesTurn then
          FNotation.Add(tkNumber, IntToStr(TempPos.MoveNumber) + '.')
        else
          FNotation.Add(tkNumber, IntToStr(TempPos.MoveNumber) + '...');
        NeedsMoveNumber := False;
      end;
      if APly.NonStandardGlyph > 0 then
        FNotation.Add(tkNAG, IntToStr(APly.NonStandardGlyph));
      FNotation.Add(tkMove, TempPos.MoveToSAN(APly.Move));
      if APly.MoveAssessment > 0 then
        FNotation.Add(tkNAG, IntToStr(APly.MoveAssessment));
      if APly.PositionalAssessment > 0 then
        FNotation.Add(tkNAG, IntToStr(APly.PositionalAssessment));
      if Length(APly.CommentTextInBehind) > 0 then
        FNotation.Add(tkComment, APly.CommentTextInBehind);
    end;

  var
    Ply: TPly;
    i: integer;
  begin
    if CurrentRoot.Children.Size = 0 then
    begin // End of line
      FNotation.Add(tkEndLine, '');
      Exit;
    end;
    TempPos := TStandardPosition.Create;
    TempPos.Copy(StartPos);
    Ply := CurrentRoot.Children.Items[0].Data;
    // Write first move of main line
    AddPlyToNotation(Ply);
    if CurrentRoot.Children.Size > 1 then
    begin
      Inc(VarLevel);
      for i := 1 to CurrentRoot.Children.Size - 1 do
      begin
        NeedsMoveNumber := True;
        TempPos.Copy(StartPos);
        Ply := CurrentRoot.Children.Items[i].Data;
        // Write side lines
        FNotation.Add(tkBeginLine, '');
        AddPlyToNotation(Ply);
        TempPos.PlayMove(Ply.Move);
        RecursiveLineToString(CurrentRoot.Children.Items[i], TempPos);
      end;
      Dec(VarLevel);
      NeedsMoveNumber := True;
    end;
    TempPos.Copy(StartPos);
    Ply := CurrentRoot.Children.Items[0].Data;
    TempPos.PlayMove(Ply.Move);
    // Write the rest of main line
    RecursiveLineToString(CurrentRoot.Children.Items[0], TempPos);
    TempPos.Free;
  end;

var
  Token: PToken;
begin
  for Token in FNotation do
    Dispose(Token);
  FNotation.Clear;
  FNotation.Add(tkBeginLine, '');
  NeedsMoveNumber := True;
  if FPlyTree.Count > 0 then
  begin
    Varlevel := 0;
    RecursiveLineToString(FPlyTree.Root, FInitialPosition as TStandardPosition);
  end;
  FNotation.Add(tkResult, GameResultToStr(GameResult));
end;

end.
