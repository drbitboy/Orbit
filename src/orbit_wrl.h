#include "orbit_wrl_defs.h"

NavigationInfo { headlight FALSE }

#ifdef _ORBIT_WRL_SHAPE_FILE_
Shape { 
#include _ORBIT_WRL_SHAPE_FILE_
  appearance Appearance { material Material { diffuseColor 1 1 1 } } 
}
#endif

/* yellow trajectory */

#define _ORBIT_WRL_TRAJECTORY_( COORD, INDEX) \
Shape { \
  geometry IndexedLineSet { \
    coord Coordinate { point [ COORD ] } coordIndex [ INDEX ] \
  } \
  appearance Appearance { material Material { diffuseColor 1 1 0 } } \
}
