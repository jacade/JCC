{ JCC (Jan's Chess Componenents) - This file contains a basic class to handle games
  Copyright (C) 2016-2017  Jan Dette

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
  Classes, SysUtils, gvector, MoveList, Position, Ply, NotationToken;

type
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

type

  { TGame }

  TGame = class
  private
    function GetCurrentPlyNumber: word;
    function GetPlyNodeWithMove(const AMove: TMove): TPlyTreeNode;
    procedure SetInitialPosition(AValue: TPosition);
  protected
    FCurrentPlyNode: TPlyTreeNode;
    FCurrentPosition: TPosition;
    FInitialPosition: TPosition;
    FPlyTree: TPlyTree;
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
    function GetGameNotation(MoveToStrOptions: TMoveToStrOptions): TGameNotation;
      virtual; abstract;
    // This returns the last tree node of in the main line of CurrentPlyNode
    function GetLastPlyNodeInCurrentVariation: TPlyTreeNode;
    function GetMovesToCurrentPosition: TMoveList;
    // Setups position before last move on board
    procedure GoOneMoveBackward;
    // Setups position after next move on board
    procedure GoOneMoveForward(const Index: integer);
    procedure GoToPositionAfterMove(const AMove: TMove);
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
    // NOTE: PlyTree.Root.Data will always be nil
    property PlyTree: TPlyTree read FPlyTree;
  end;

  { TStandardGame }

  TStandardGame = class(TGame)
  private
    procedure SetGameResult(AValue: TGameResult);
  protected
    FGameResult: TGameResult;
  public
    constructor Create; override;
    constructor Create(const AInitialPosition: TPosition); override;
    function GetGameNotation(MoveToStrOptions: TMoveToStrOptions): TGameNotation;
      override;
  public
    property GameResult: TGameResult read FGameResult write SetGameResult;
  end;

function StrToFIDETitle(const s: string): TFIDETitle;
function StrToPlayerType(const s: string): TPlayerType;
function StrToModeType(const s: string): TModeType;
function StrToTerminationType(const s: string): TTerminationType;

implementation

procedure DeletePly(const AData: TPly);
begin
  AData.Free;
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

{ TGame }

function TGame.GetCurrentPlyNumber: word;
var
  Temp: TPlyTreeNodeList;
begin
  Temp := FPlyTree.GetPathTo(FCurrentPlyNode);
  Result := Temp.Size - 1;
  Temp.Free;
end;

function TGame.GetPlyNodeWithMove(const AMove: TMove): TPlyTreeNode;
var
  FoundNode: TPlyTreeNode;

  function Search(const Root: TPlyTreeNode): boolean;
  var
    i: integer;
  begin
    Result := False;
    if Assigned(Root) then
    begin
      for i := 0 to Root.Children.Size - 1 do
      begin
        if Root.Children[i].Data.Move = AMove then
        begin
          FoundNode := Root.Children[i];
          Exit(True);
        end;
        if Search(Root.Children[i]) then
          Exit(True);
      end;
    end;
  end;

begin
  FoundNode := nil;
  Search(FPlyTree.Root);
  Result := FoundNode;
end;

procedure TGame.SetInitialPosition(AValue: TPosition);
begin
  if FInitialPosition = AValue then
    Exit;
  if FPlyTree.Count > 0 then
    raise Exception.Create('Game already began, cannot change initial position');
  FInitialPosition := AValue;
  FCurrentPosition.Copy(FInitialPosition);
end;

procedure TGame.AddMove(AMove: TMove);
begin
  FCurrentPlyNode.Children.PushBack(TPlyTreeNode.Create(TPly.Create(AMove)));
  FCurrentPlyNode := FCurrentPlyNode.Children.Items[0];
  FCurrentPosition.PlayMove(AMove);
end;

procedure TGame.AddMoveAsNewMainLine(AMove: TMove);
begin
  FCurrentPlyNode.Children.Insert(0, TPlyTreeNode.Create(TPly.Create(AMove)));
  FCurrentPlyNode := FCurrentPlyNode.Children.Items[0];
  FCurrentPosition.PlayMove(AMove);
end;

procedure TGame.AddMoveAsSideLine(AMove: TMove);
begin
  FCurrentPlyNode.Children.PushBack(TPlyTreeNode.Create(TPly.Create(AMove)));
  FCurrentPlyNode := FCurrentPlyNode.Children.Back;
  FCurrentPosition.PlayMove(AMove);
end;

constructor TGame.Create;
begin
  FPlyTree := TPlyTree.Create;
  FPlyTree.Root := TPlyTreeNode.Create(nil);
  FCurrentPlyNode := FPlyTree.Root;
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
end;

destructor TGame.Destroy;
begin
  // We need to delete all plies currently stored in the tree
  FPlyTree.DepthFirstTraverse(@DeletePly);
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

function TGame.GetMovesToCurrentPosition: TMoveList;
var
  Temp: TPlyTreeNodeList;
  Node: TPlyTreeNode;
begin
  Temp := FPlyTree.GetPathTo(CurrentPlyNode);
  if Temp.Size > 0 then
  begin
    Result := TMoveList.Create;
    for Node in Temp do
      if Node.Data <> nil then
        Result.Add(Node.Data.Move.Copy);
  end;
  Temp.Free;
end;

procedure TGame.GoOneMoveBackward;
begin
  GoToPositionAfterPlyNode(FPlyTree.GetParentOf(FCurrentPlyNode));
end;

procedure TGame.GoOneMoveForward(const Index: integer);
begin
  GoToPositionAfterPlyNode(FCurrentPlyNode.Children.Items[Index]);
end;

procedure TGame.GoToPositionAfterMove(const AMove: TMove);
begin
  GoToPositionAfterPlyNode(GetPlyNodeWithMove(AMove));
end;

procedure TGame.GoToPositionAfterPlyNode(const APlyTreeNode: TPlyTreeNode);
var
  Temp: TPlyTreeNodeList;
  Node: TPlyTreeNode;
begin
  Temp := FPlyTree.GetPathTo(APlyTreeNode);
  if Temp.Size = 0 then
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
  FInitialPosition.Free;
  FInitialPosition := AInitialPosition;
  FCurrentPosition.Copy(FInitialPosition);
end;

procedure TStandardGame.SetGameResult(AValue: TGameResult);
begin
  if FGameResult = AValue then
    Exit;
  FGameResult := AValue;
end;

function TStandardGame.GetGameNotation(MoveToStrOptions: TMoveToStrOptions):
TGameNotation;
var
  VarLevel: word;
  Notation: TGameNotation;
  NeedsMoveNumber: boolean;

  procedure RecursiveCreateNotation(CurrentRoot: TPlyTreeNode; StartPos: TPosition);
  var
    TempPos: TPosition;

    procedure AddPlyToNotation(const APly: TPly);
    begin
      if Length(APly.CommentTextInFront) > 0 then
      begin
        Notation.Add(TCommentToken.Create(APly.CommentTextInFront));
        NeedsMoveNumber := True;
      end;
      if NeedsMoveNumber or TempPos.WhitesTurn then
      begin
        if TempPos.WhitesTurn then
          Notation.Add(TMoveNumberToken.Create(IntToStr(TempPos.MoveNumber) + '.'))
        else
          Notation.Add(TMoveNumberToken.Create(IntToStr(TempPos.MoveNumber) + '...'));
        NeedsMoveNumber := False;
      end;
      if APly.NonStandardGlyph > 0 then
        Notation.Add(TNAGToken.Create(APly.NonStandardGlyph,
          NAGToStr(APly.NonStandardGlyph)));
      Notation.Add(TMoveToken.Create(APly.Move,
        TempPos.MoveToStr(APly.Move, MoveToStrOptions)));
      if APly.MoveAssessment > 0 then
        Notation.Add(TNAGToken.Create(APly.MoveAssessment,
          NAGToStr(APly.MoveAssessment)));
      if APly.PositionalAssessment > 0 then
        Notation.Add(TNAGToken.Create(APly.PositionalAssessment,
          NAGToStr(APly.PositionalAssessment)));
      if Length(APly.CommentTextInBehind) > 0 then
      begin
        Notation.Add(TCommentToken.Create(APly.CommentTextInBehind));
        NeedsMoveNumber := True;
      end;
    end;

  var
    Ply: TPly;
    i: integer;
  begin
    if CurrentRoot.Children.Size = 0 then
    begin // End of line
      Notation.Add(TEndLineToken.Create);
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
        Notation.Add(TBeginLineToken.Create);
        AddPlyToNotation(Ply);
        TempPos.PlayMove(Ply.Move);
        RecursiveCreateNotation(CurrentRoot.Children.Items[i], TempPos);
      end;
      Dec(VarLevel);
      NeedsMoveNumber := True;
    end;
    TempPos.Copy(StartPos);
    Ply := CurrentRoot.Children.Items[0].Data;
    TempPos.PlayMove(Ply.Move);
    // Write the rest of main line
    RecursiveCreateNotation(CurrentRoot.Children.Items[0], TempPos);
    TempPos.Free;
  end;

begin
  Notation := TGameNotation.Create(True);
  Notation.Add(TBeginLineToken.Create);
  NeedsMoveNumber := True;
  if FPlyTree.Count > 0 then
  begin
    Varlevel := 0;
    RecursiveCreateNotation(FPlyTree.Root, FInitialPosition as TStandardPosition);
  end;
  Notation.Add(TResultToken.Create(GameResult));
  Result := Notation;
end;

end.
