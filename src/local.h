/* local.h */
#ifndef _ORBIT_LOCAL_H_
#define _ORBIT_LOCAL_H_

#if XtSpecificationRelease < 5
#define XtSetLanguageProc(A,B,C)
#define XmStringCreateLocalized( A) \
        XmStringCreateLtoR( A, XmSTRING_DEFAULT_CHARSET)
#define XmFONTLIST_DEFAULT_TAG XmSTRING_DEFAULT_CHARSET
#endif

/* sometimes a return in the middle of a routine causes a problem with
 * the HP-UX assembler
 */
#if defined(__GNUC__) && defined(__HPUX_ASM__) && defined(__hp9000s300__)
#define LOCAL_LONGRETURN exit(0)
#else
#define LOCAL_LONGRETURN return
#endif

#endif /* #ifndef _ORBIT_LOCAL_H_ */
