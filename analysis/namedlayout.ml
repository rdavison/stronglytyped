open! Core
open! Bonsai
open! Bonsai.Let_syntax

let qwerty =
  Key.Id.Map.of_alist_exn
    [ `Q, `Alpha 'q'
    ; `W, `Alpha 'w'
    ; `E, `Alpha 'e'
    ; `R, `Alpha 'r'
    ; `T, `Alpha 't'
    ; `Y, `Alpha 'y'
    ; `U, `Alpha 'u'
    ; `I, `Alpha 'i'
    ; `O, `Alpha 'o'
    ; `P, `Alpha 'p'
    ; `A, `Alpha 'a'
    ; `S, `Alpha 's'
    ; `D, `Alpha 'd'
    ; `F, `Alpha 'f'
    ; `G, `Alpha 'g'
    ; `H, `Alpha 'h'
    ; `J, `Alpha 'j'
    ; `K, `Alpha 'k'
    ; `L, `Alpha 'l'
    ; `SEMICOLON, `Sym (';', ':')
    ; `Z, `Alpha 'z'
    ; `X, `Alpha 'x'
    ; `C, `Alpha 'c'
    ; `V, `Alpha 'v'
    ; `B, `Alpha 'b'
    ; `N, `Alpha 'n'
    ; `M, `Alpha 'm'
    ; `COMMA, `Sym (',', '<')
    ; `PERIOD, `Sym ('.', '>')
    ; `SLASH, `Sym ('/', '?')
    ]
;;

let dvorak =
  Key.Id.Map.of_alist_exn
    [ `HYPHEN, `Sym ('[', '{')
    ; `EQUAL, `Sym (']', '}')
    ; `Q, `Sym ('\'', '"')
    ; `W, `Sym (',', '<')
    ; `E, `Sym ('.', '>')
    ; `R, `Alpha 'p'
    ; `T, `Alpha 'y'
    ; `Y, `Alpha 'f'
    ; `U, `Alpha 'g'
    ; `I, `Alpha 'c'
    ; `O, `Alpha 'r'
    ; `P, `Alpha 'l'
    ; `OPEN_BRACKET, `Sym ('/', '?')
    ; `CLOSE_BRACKET, `Sym ('=', '+')
    ; `A, `Alpha 'a'
    ; `S, `Alpha 'o'
    ; `D, `Alpha 'e'
    ; `F, `Alpha 'u'
    ; `G, `Alpha 'i'
    ; `H, `Alpha 'd'
    ; `J, `Alpha 'h'
    ; `K, `Alpha 't'
    ; `L, `Alpha 'n'
    ; `SEMICOLON, `Alpha 's'
    ; `QUOTE, `Sym ('-', '_')
    ; `Z, `Sym (';', ':')
    ; `X, `Alpha 'q'
    ; `C, `Alpha 'j'
    ; `V, `Alpha 'k'
    ; `B, `Alpha 'x'
    ; `N, `Alpha 'b'
    ; `M, `Alpha 'm'
    ; `COMMA, `Alpha 'w'
    ; `PERIOD, `Alpha 'v'
    ; `SLASH, `Alpha 'z'
    ]
;;

let colemak =
  Key.Id.Map.of_alist_exn
    [ `Q, `Alpha 'q'
    ; `W, `Alpha 'w'
    ; `E, `Alpha 'f'
    ; `R, `Alpha 'p'
    ; `T, `Alpha 'g'
    ; `Y, `Alpha 'j'
    ; `U, `Alpha 'l'
    ; `I, `Alpha 'u'
    ; `O, `Alpha 'y'
    ; `P, `Sym (';', ':')
    ; `A, `Alpha 'a'
    ; `S, `Alpha 'r'
    ; `D, `Alpha 's'
    ; `F, `Alpha 't'
    ; `G, `Alpha 'd'
    ; `H, `Alpha 'h'
    ; `J, `Alpha 'n'
    ; `K, `Alpha 'e'
    ; `L, `Alpha 'i'
    ; `SEMICOLON, `Alpha 'o'
    ; `Z, `Alpha 'z'
    ; `X, `Alpha 'x'
    ; `C, `Alpha 'c'
    ; `V, `Alpha 'v'
    ; `B, `Alpha 'b'
    ; `N, `Alpha 'k'
    ; `M, `Alpha 'm'
    ; `COMMA, `Sym (',', '<')
    ; `PERIOD, `Sym ('.', '>')
    ; `SLASH, `Sym ('/', '?')
    ]
;;
