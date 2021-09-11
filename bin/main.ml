let () = 
  let image_width = 256 in
  let image_height = 256 in
  let oc = open_out "image.ppm" in
  Printf.fprintf oc "P3\n%d %d\n255\n" image_width image_height;
  for j = (image_height - 1) downto 0 do
    for i = 0 to (image_width - 1) do
      let r =  Float.of_int(i) /. (Float.of_int(image_width) -. 1.0)
      and g =  Float.of_int(j) /. (Float.of_int(image_height) -. 1.0)
      and b = 0.25 in
      let ir = Int.of_float( 255.999 *. r )
      and ig = Int.of_float( 255.999 *. g )
      and ib = Int.of_float( 255.999 *. b ) in
      Printf.fprintf oc "%d %d %d\n" ir ig ib
    done
  done;

  close_out oc;