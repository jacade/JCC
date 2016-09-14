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
  Classes, SysUtils, MoveList, Position, Ply;

type

  { TGame }

  TGame = class
  private
    function GetCurrentPlyNumber: word;
    procedure SetInitialPosition(AValue: TPosition);
  protected
    //  FBoard: TBoard;
    FCurrentPlyNode: TPlyTreeNode;
    FCurrentPosition: TPosition;
    FInitialPosition: TPosition;
    FPlyTree: TPlyTree;
    function GetNotation: string; virtual;
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
    property Notation: string read GetNotation;
    // NOTE: PlyTree.Root.Data will always be nil
    property PlyTree: TPlyTree read FPlyTree;
  end;

  { TStandardGame }

  TStandardGame = class(TGame)
  protected
    function GetNotation: string; override;
  public
    constructor Create; override;
    constructor Create(const AInitialPosition: TPosition); override;
  end;

implementation

procedure DeletePly(const AData: TPly);
begin
  AData.Free;
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

function TGame.GetNotation: string;
begin
  Result := '';
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

function TStandardGame.GetNotation: string;
var
  VarLevel: word;

  function RecursiveLineToString(CurrentRoot: TPlyTreeNode;
    StartPos: TStandardPosition): string;
  var
    TempPos: TStandardPosition;

    function PlyToStr(APly: TPly; FirstMoveInVariation: Boolean): string;
    begin
      Result := '';
      if Length(APly.CommentTextInFront) > 0 then
        Result := Result + APly.CommentTextInFront + ' ';
      if APly.NonStandardGlyph > 0 then
        Result := Result + NAGToStr(APly.NonStandardGlyph) + ' ';
      if TempPos.WhitesTurn then
        Result := Result + IntToStr(TempPos.MoveNumber) + '.'
      else
      if FirstMoveInVariation then
        Result := Result + IntToStr(TempPos.MoveNumber) + '...';
      Result := Result + TempPos.MoveToSAN(APly.Move);
      if APly.MoveAssessment > 0 then
        Result := Result + NAGToStr(APly.MoveAssessment) + ' ';
      if APly.PositionalAssessment > 0 then
        Result := Result + NAGToStr(APly.PositionalAssessment) + ' ';
      if Result[Length(Result)] <> ' ' then
        Result := Result + ' ';
      if Length(APly.CommentTextInBehind) > 0 then
        Result := Result + APly.CommentTextInBehind + ' ';
    end;

  var
    Ply: TPly;
    i: integer;
  begin
    Result := '';
    if CurrentRoot.Children.Size = 0 then
    begin // End of line
      if VarLevel = 1 then
        Result := '] ' + LineEnding
      else
      if VarLevel > 1 then
        Result := ') ';
      Exit;
    end;
    TempPos := TStandardPosition.Create;
    TempPos.Copy(StartPos);
    Ply := CurrentRoot.Children.Items[0].Data;
    // Write first move of main line
    Result := Result + PlyToStr(Ply, False);
    if CurrentRoot.Children.Size > 1 then
    begin
      Inc(VarLevel);
      for i := 1 to CurrentRoot.Children.Size - 1 do
      begin
        TempPos.Copy(StartPos);
        Ply := CurrentRoot.Children.Items[i].Data;
        // Write side lines
        if VarLevel = 1 then
          Result := Result + LineEnding + StringOfChar(' ', 2 * VarLevel) + '['
        else
        if VarLevel > 1 then
          Result := Result + '(';
        Result := Result + PlyToStr(Ply, True);
        TempPos.PlayMove(Ply.Move);
        Result := Result + RecursiveLineToString(CurrentRoot.Children.Items[i], TempPos);
      end;
      Dec(VarLevel);
    end;
    TempPos.Copy(StartPos);
    Ply := CurrentRoot.Children.Items[0].Data;
    TempPos.PlayMove(Ply.Move);
    // Write the rest of main line
    Result := Result + RecursiveLineToString(CurrentRoot.Children.Items[0], TempPos);
    TempPos.Free;
  end;

begin
  Result := inherited GetNotation;
  if FPlyTree.Count > 0 then
  begin
    Varlevel := 0;
    Result := RecursiveLineToString(FPlyTree.Root, FInitialPosition as
      TStandardPosition);
  end;
end;

constructor TStandardGame.Create;
begin
  inherited Create;
  FInitialPosition := TStandardPosition.Create;
  FCurrentPosition := TStandardPosition.Create;
end;

constructor TStandardGame.Create(const AInitialPosition: TPosition);
begin
  Create;
  FInitialPosition.Copy(AInitialPosition);
  FCurrentPosition.Copy(AInitialPosition);
end;

end.
