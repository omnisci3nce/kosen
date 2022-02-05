open Kosen
open Kosen.Defaultscene
open Kosen.Utils
open Kosen.Raytracer

let () =
  let samples_per_pixel = 4 in
  let oc = open_out "image.ppm" in
  Printf.fprintf oc "P3\n%d %d\n255\n" image_width image_height;

  (* Iterate through every pixel in row, for each row *)
  for j = (image_height - 1) downto 0 do
    Printf.printf "Scanlines remaining: %d\n" j; flush stdout;
    for i = 0 to (image_width - 1) do

      let color_total =
        List.fold_left (fun (acc: Vec3.t) _ -> 
          let u = (Float.of_int(i) +. Random.float(1.)) /. Float.of_int(image_width)
          and v = (Float.of_int(j) +. Random.float(1.)) /. Float.of_int(image_height) in
          let r = camera |> Camera.get_ray u v in
          Vec3.add (ray_color (r: Ray.t) world) acc
        ) (Vec3.create 0. 0. 0.) (range 0 (samples_per_pixel - 1)) in
   
      Color.write_color oc color_total samples_per_pixel
    done
  done;

  close_out oc;
