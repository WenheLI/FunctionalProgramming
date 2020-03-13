destruct :: ((String, String), String) -> (String, String, String)

destruct x = let 
                (first, second) = x
             in
                (fst first, snd first, second)

construct :: ((String, String), String) -> (String, (String ,String))

construct x = let
                 (first, second, third) = destruct x
              in
                  (first, (second, third))