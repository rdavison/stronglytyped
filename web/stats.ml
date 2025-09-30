open! Import

let component ~same_finger_vdom _graph =
  let%arr same_finger_vdom = same_finger_vdom in
  Vdom.Node.div
    [ Vdom.Node.h2 [ Vdom.Node.text "Same Finger Analysis" ]; same_finger_vdom ]
;;
