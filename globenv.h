/*=================================================================*/
/* The following functions are defined in the GlobEnv dynalink.    */
/*=================================================================*/
extern int far pascal GLBENTER(char far * thename);
extern int far pascal GLBDELETE(char far * thename);
extern int far pascal GLBQUERY(char far * thename
                              ,unsigned far * thesize);
extern int far pascal GLBSTORE(char far * thename
                              ,void far * thevalue
                              ,unsigned thelen);
extern int far pascal GLBFETCH( char far * thename
                              , void far * valbuff
                              , unsigned far * thesize);
extern int far pascal GLBNEXT ( char far * thename
                              , int strmax
                              , char far * strbuff
                              , unsigned far * thesize);
