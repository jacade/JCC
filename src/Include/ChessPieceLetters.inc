// This file contains translations for various languages of piece names.
// Based on https://en.wikipedia.org/wiki/Algebraic_notation_(chess)#Piece_names_in_various_languages
// The language codes are from ISO 639-1 as shown on https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes

type
  TChessPieceLetters = array[1..12] of string;

const
  // chesspiece identifier letters pawn, knight, bishop, rook, queen, king
  // First 5 letters are for the white pieces, the latter 5 for the black ones
  // TODO: Could this be optimized easily?

  // Afrikaans
  PieceLetters_AF: TChessPieceLetters =
    ('P', 'R', 'L', 'T', 'D', 'K', 'P', 'R', 'L', 'T', 'D', 'K');
  // Albanian
  PieceLetters_SQ: TChessPieceLetters =
    ('U', 'Ka', 'O', 'Ku', 'Msh', 'M', 'U', 'Ka', 'O', 'Ku', 'Msh', 'M');
  // Armenian
  PieceLetters_HY: TChessPieceLetters =
    ('Զ', 'Ձ', 'Փ', 'Ն', 'Թ', 'Ա', 'Զ', 'Ձ', 'Փ', 'Ն', 'Թ', 'Ա');
  // Basque
  PieceLetters_EU: TChessPieceLetters =
    ('P', 'Z', 'A', 'G', 'D', 'E', 'P', 'Z', 'A', 'G', 'D', 'E');
  // Belarusian
  PieceLetters_BE: TChessPieceLetters =
    ('Л', 'В', 'А', 'Лд', 'Вз', 'K', 'Л', 'В', 'А', 'Лд', 'Вз', 'K');
  // Bengali
  PieceLetters_BN: TChessPieceLetters =
    ('B', 'G', 'H', 'N', 'M', 'R', 'B', 'G', 'H', 'N', 'M', 'R');
  // Bulgarian
  PieceLetters_BG: TChessPieceLetters =
    ('П', 'К', 'О', 'Т', 'Д', 'Ц', 'П', 'К', 'О', 'Т', 'Д', 'Ц');
  // Catalan
  PieceLetters_CA: TChessPieceLetters =
    ('P', 'C', 'A', 'T', 'D', 'R', 'P', 'C', 'A', 'T', 'D', 'R');
  // Chinese
  PieceLetters_ZH: TChessPieceLetters =
    ('P', 'N', 'B', 'R', 'Q', 'K', 'P', 'N', 'B', 'R', 'Q', 'K');
  // Czech
  PieceLetters_CS: TChessPieceLetters =
    ('P', 'J', 'S', 'V', 'D', 'K', 'P', 'J', 'S', 'V', 'D', 'K');
  // Danish
  PieceLetters_DA: TChessPieceLetters =
    ('B', 'S', 'L', 'T', 'D', 'K', 'B', 'S', 'L', 'T', 'D', 'K');
  // Dutch
  PieceLetters_NL: TChessPieceLetters =
    ('pi', 'P', 'L', 'T', 'D', 'K', 'pi', 'P', 'L', 'T', 'D', 'K');
  // English
  PieceLetters_EN: TChessPieceLetters =
    ('P', 'N', 'B', 'R', 'Q', 'K', 'P', 'N', 'B', 'R', 'Q', 'K');
  // Esperanto
  PieceLetters_EO: TChessPieceLetters =
    ('P', 'Ĉ', 'K', 'T', 'D', 'R', 'P', 'Ĉ', 'K', 'T', 'D', 'R');
  // Estonian
  PieceLetters_ET: TChessPieceLetters =
    ('E', 'R', 'O', 'V', 'L', 'K', 'E', 'R', 'O', 'V', 'L', 'K');
  // Finnish
  PieceLetters_FI: TChessPieceLetters =
    ('S', 'R', 'L', 'T', 'D', 'K', 'S', 'R', 'L', 'T', 'D', 'K');
  // French
  PieceLetters_FR: TChessPieceLetters =
    ('P', 'C', 'F', 'T', 'D', 'R', 'P', 'C', 'F', 'T', 'D', 'R');
  // Georgian
  PieceLetters_KA: TChessPieceLetters =
    ('პ', 'მ', 'კ', 'ე', 'ლ', 'მ', 'პ', 'მ', 'კ', 'ე', 'ლ', 'მ');
  // German
  PieceLetters_DE: TChessPieceLetters =
    ('B', 'S', 'L', 'T', 'D', 'K', 'B', 'S', 'L', 'T', 'D', 'K');
  // Greek
  PieceLetters_EL: TChessPieceLetters =
    ('Σ', 'Ι', 'Α', 'Π', 'Β', 'Ρ', 'Σ', 'Ι', 'Α', 'Π', 'Β', 'Ρ');
  // Hindi
  PieceLetters_HI: TChessPieceLetters =
    ('P', 'G', 'O', 'H', 'V', 'R', 'P', 'G', 'O', 'H', 'V', 'R');
  // TODO: Continue...
  // Figurine
  PieceLetters_Figurine: TChessPieceLetters =
    ('♙', '♘', '♗', '♖', '♕', '♔', '♟', '♞', '♝', '♜', '♛', '♚');
