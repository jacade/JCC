{ JCC (Jan's Chess Componenents) - This file contains classes to save and handle plies
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

unit Ply;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gtree, gstack, MoveList;

const
  MoveAssessments = [1..9];
  PositionalAssessments = [10..135];
  TimePressureComments = [136..139];
  NonStandardGlyphs = [140..255];

type

  TNAG = byte;   // numeric Annotation Glyph

  { TPly }

  TPly = class
  private
    FCommentTextInBehind: string;
    FCommentTextInFront: string;
    FMove: TMove;
    FMoveAssessment: TNAG; // 1-9
    FPositionalAssessment: TNAG; // 10-135
    FTimePressureCommentary: TNAG; // 136-139
    FNonStandardGlyph: TNAG;  // 140-255
  public
    constructor Create(const AMove: TMove);
    destructor Destroy; override;
  public
    property CommentTextInBehind: string read FCommentTextInBehind
      write FCommentTextInBehind;
    property CommentTextInFront: string read FCommentTextInFront
      write FCommentTextInFront;
    property Move: TMove read FMove write FMove;
    property MoveAssessment: TNAG read FMoveAssessment write FMoveAssessment;
    property PositionalAssessment: TNAG read FPositionalAssessment
      write FPositionalAssessment;
    property TimePressureCommentary: TNAG read FTimePressureCommentary
      write FTimePressureCommentary;
    property NonStandardGlyph: TNAG read FNonStandardGlyph write FNonStandardGlyph;
  end;

  TPlyTree = specialize TTree<TPly>;
  TPlyTreeNode = TPlyTree.TTreeNodeType;
  TPlyTreeNodeList = TPlyTreeNode.TTreeNodeList;
  TPlyTreeNodeStack = specialize TStack<TPlyTreeNode>;

  { TPlyTreeHelper }

  TPlyTreeHelper = class helper for TPlyTree
    // Counts the nodes in the tree
    function Count: word;
    // Returns the parent node of a given node
    function GetParentOf(const APlyTreeNode: TPlyTreeNode): TPlyTreeNode;
    // Returns the path in the tree to the given node
    function GetPathTo(const APlyTreeNode: TPlyTreeNode): TPlyTreeNodeList;
  end;

function NAGToStr(const ANAG: TNAG): string;

implementation

function NAGToStr(const ANAG: TNAG): string;
begin
  // This is based on https://en.wikipedia.org/wiki/Numeric_Annotation_Glyphs
  case ANAG of
    // Standard
    0: Result := '';
    1: Result := '!';
    2: Result := '?';
    3: Result := '!!';
    4: Result := '??';
    5: Result := '!?';
    6: Result := '?!';
    7: Result := '□';
    10: Result := '=';
    13: Result := '∞';
    14: Result := '⩲';
    15: Result := '⩱';
    16: Result := '±';
    17: Result := '∓';
    18: Result := '+-';
    19: Result := '-+';
    22, 23: Result := '⨀';
    32, 33: Result := '⟳';
    36, 37: Result := '→';
    40, 41: Result := '↑';
    132, 133: Result := '⇆';
    // Non-standard
    140: Result := '∆';
    141: Result := '∇';
    142: Result := '⌓';
    145: Result := 'RR';
    146: Result := 'N';
    239: Result := '⇔';
    240: Result := '⇗';
    242: Result := '⟫';
    243: Result := '⟪';
    244: Result := '✕';
    245: Result := '⊥';
    else
      Result := '$' + IntToStr(ANAG);
  end;
end;

{ TPlyTreeHelper }

function TPlyTreeHelper.Count: word;

  function RecursiveCount(CurrentNode: TPlyTreeNode): word;
  var
    Child: TPlyTreeNode;
  begin
    Result := 0;
    if CurrentNode = nil then
      Exit;
    for Child in CurrentNode.Children do
    begin
      Result := Result + RecursiveCount(Child);
    end;
    Result := Result + 1;
  end;

begin
  Result := RecursiveCount(Self.Root);
end;

function TPlyTreeHelper.GetParentOf(const APlyTreeNode: TPlyTreeNode): TPlyTreeNode;

  function RecursiveSearch(CurrentRoot: TPlyTreeNode): TPlyTreeNode;
  var
    Child: TPlyTreeNode;
  begin
    Result := nil;
    if (CurrentRoot = nil) then
      Exit;
    for Child in CurrentRoot.Children do
    begin
      if Child = APlyTreeNode then
        Result := CurrentRoot;
    end;
    if Result = nil then
    begin
      for Child in CurrentRoot.Children do
      begin
        Result := RecursiveSearch(Child);
        if Result <> nil then
          Break;
      end;
    end;
  end;

begin
  Result := RecursiveSearch(Self.Root);
end;

function TPlyTreeHelper.GetPathTo(const APlyTreeNode: TPlyTreeNode): TPlyTreeNodeList;

  function RecursiveSearch(CurrentRoot: TPlyTreeNode;
    CurrentPath: TPlyTreeNodeList): boolean;
  var
    Child: TPlyTreeNode;
  begin
    Result := False;
    if CurrentRoot = nil then
      Exit;
    if CurrentRoot = APlyTreeNode then
    begin
      CurrentPath.Insert(0, CurrentRoot);
      Result := True;
      Exit;
    end;
    for Child in CurrentRoot.Children do
    begin
      if RecursiveSearch(Child, CurrentPath) then
      begin
        CurrentPath.Insert(0, CurrentRoot);
        Result := True;
        Exit;
      end;
    end;
  end;

begin
  Result := TPlyTreeNodeList.Create;
  RecursiveSearch(Self.Root, Result);
end;

{ TPly }

constructor TPly.Create(const AMove: TMove);
begin
  FMove := AMove;
  FMoveAssessment := 0;
  FPositionalAssessment := 0;
  FTimePressureCommentary := 0;
  FNonStandardGlyph := 0;
end;

destructor TPly.Destroy;
begin
  inherited Destroy;
end;

end.
