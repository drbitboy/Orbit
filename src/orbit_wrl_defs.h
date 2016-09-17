/* Instrument FOV parameters:
 *   LOC     X Y Z      offset to center of FOV, Asteroid Body Fixed Coords
 *   OR      X Y Z Rot  orientation of FOV, ABF from VRML base (X right, Y up)
 *   SCPOS   X Y Z      spacecraft position, ABF
 *   R       R          range to spacecraft from FOV
 *   FROMSUN X Y Z      directional light, ABF
 *   TXT     abc        text to display in instrument FOV
 *   URL     #abc       "#" + TXT - used for Anchor url
 *   DL      Dl_6/99... DirectionalLight nodeID
 *   VP      Vp_6/99... Viewpoint nodeID
 *
 * ANY FOV parameters:
 *   LOC to URL:  same as above
 *   INSTRFOV:    HORIZ VERT    actual instrument FOV size, radians
 *   VPFOV:       FOV           Viewpoint FOV size, set to X * INSTRFOV (X ~ 10)
 */

/* MSI */
#define MSIFov( LOC,OR,SCPOS,R,FROMSUN,TXT,URL,DL,VP) \
  ANYFov( LOC,OR,SCPOS,R,FROMSUN,TXT,URL,DL,VP, .0514983 .039284, .42)

/* NIS Narrow */
#define NISFov( LOC,OR,SCPOS,R,FROMSUN,TXT,URL,DL,VP) \
  ANYFov(LOC,OR,SCPOS,R,FROMSUN,TXT,URL,DL,VP, .006981317 .013264502, .14)

/* NIS Wide */
#define NIS2Fov NISWIDEFov
#define NISWIDEFov( LOC,OR,SCPOS,R,FROMSUN,TXT,URL,DL,VP) \
  ANYFov( LOC,OR,SCPOS,R,FROMSUN,TXT,URL,DL,VP, .013962634 .013264502, .14)

#define ANYFov( LOC,OR,SCPOS,R,FROMSUN,TXT,URL,DL,VP,INSTRFOV,VPFOV) \
  /* Viewpoint w/ FOV a multiple of instrument FOV */ \
  /*   - rotate to S/C view, translate to S/C position */ \
  DEF VP Viewpoint { position SCPOS orientation OR \
    description TXT jump TRUE fieldOfView VPFOV \
  } \
  DEF DL DirectionalLight { direction FROMSUN on FALSE } \
  ROUTE VP.isBound TO DL.on /* connect lighting to viewpoint */ \
  Anchor { url URL children [ \
      /* Anchor child:  instrument FOV frame + Text */ \
      /*  - scale 1x1 FOV by S/C range (km) & instrument FOV (Radians) */ \
      /*  - rotated to S/C orientation */ \
      /*  - translated to FOV frame center location */ \
      Transform { scale R R 1  rotation OR  translation LOC \
        children [ \
          /* Scale Text & 1x1 FOV frame to actual FOV size in radians */ \
          Transform { scale INSTRFOV 1 children [ \
              /* - TXT - scale to fraction (.7) of FOV width (1) */ \
              /*       - translate to top left of FOV (+Y, -X) */ \
              Transform { \
                scale .7 .1 1 \
                translation -.35 .38 0 \
                children [ \
                  Shape { geometry Text { string [ TXT ] length [ 1 ] } \
                    appearance Appearance { \
                      material Material {emissiveColor 0 1 1 } \
              } } ] } \
              /* - FOV - in XY plane before transforms above */ \
              /*       - size is 1x1 centered on origin */ \
              /*       - color is cyan */ \
              Shape { geometry IndexedLineSet { \
                  coord Coordinate { point [ \
                    -.5 .5 0   .5 .5 0   .5 -.5 0   -.5 -.5 0 ] } \
                  coordIndex [ 0 1 2 3 0 -1 ] } \
                appearance Appearance { \
                  material Material { emissiveColor 0 1 1 } \
  } } ] } ] } ] }
