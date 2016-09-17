#ifndef _ORBIT3D_DUMMY_FUNCS_H_
#define _ORBIT3D_DUMMY_FUNCS_H_

#define DUMYROUTINE( A, B) \
void A () \
{ fprintf( stderr, "Call to non-existent routine:  %s\n", B); exit(1); } 

DUMYROUTINE( orbitgui_return_spudv_sc, "orbitgui_return_spudv_sc")
DUMYROUTINE( orbitgui_update_BoreRoll, "orbitgui_update_BoreRoll")
DUMYROUTINE( orbitgui_return_boreroll, "orbitgui_return_boreroll")
DUMYROUTINE( orbitgui_get1stCASFromCurItem, "orbitgui_get1stCASFromCurItem")
DUMYROUTINE( orbit_CAS_ds40Vec, "orbit_CAS_ds40Vec")
DUMYROUTINE( orbit_CAS_ds56Vec, "orbit_CAS_ds56Vec")
DUMYROUTINE( orbit_CAS_TypeToName, "orbit_CAS_ds56Vec")
DUMYROUTINE( orbitgui_add_comment_sc, "orbitgui_add_comment_sc")

#endif /* _ORBIT3D_DUMMY_FUNCS_H_ */
