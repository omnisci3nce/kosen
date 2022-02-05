open Kosen
open Kosen.Defaultscene
open Kosen.Utils
open Kosen.Raytracer
open Domainslib

let () =
  let samples_per_pixel = 100 in
  let pool = Task.setup_pool ~num_additional_domains:5 () in
  let oc = open_out "image.ppm" in
  Printf.fprintf oc "P3\n%d %d\n255\n" image_width image_height;

  let res = Array.make (image_height * image_width) Vec3.zero in
  Task.run pool (fun () ->
    Task.parallel_for pool ~start:0 ~finish:(image_height-1) ~body:(fun j ->
      (* Iterate through every pixel in row, for each row *)
        Printf.printf "Scanlines remaining: %d\n" j; flush stdout;
        for i = 0 to (image_width - 1) do
    
          let color_total =
            List.fold_left (fun (acc: Vec3.t) _ -> 
              let u = (Float.of_int(i) +. Random.float(1.)) /. Float.of_int(image_width)
              and v = (Float.of_int(image_height - j) +. Random.float(1.)) /. Float.of_int(image_height) in
              let r = camera |> Camera.get_ray u v in
              Vec3.add (ray_color (r: Ray.t) world) acc
            ) (Vec3.create 0. 0. 0.) (range 0 (samples_per_pixel - 1)) in
       
          res.(j * image_width + i) <- color_total
          
        done
      (* done; *)
      );
    
    );
  Array.iter (fun color -> Color.write_color oc color samples_per_pixel) res;

  Task.teardown_pool pool;
  close_out oc;