open Types

type face_direction = FrontFace | BackFace
type hit_record = {
  p: point;
  normal: direction;
  t: float;
  (* material: t; *)
  front_face: bool
}