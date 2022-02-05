open Types

type face_direction = FrontFace | BackFace

type hit_record = {
  p: point;
  normal: direction;
  t: float;
  front_face: bool
  (* material: t; *)
}
