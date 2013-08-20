/*=========================================================================*/
/*                                                                         */
/*              GLOBLOAD : Load Global Environment from StdIn              */
/*                                                                         */
/* The Global Environment for OS/2 is a store of named values that is      */
/* maintained by dynalink code defined in GLOBENV.ASM/.OBJ/.DLL/.LIB.      */
/* This command loads variables from a named file.  Its primary use is to  */
/* bulk-load a set of variables during system initialization.              */
/*                                                                         */
/* See also GLOBAL (which can be used to create an input file for this     */
/* program) and GLOB2SET.                                                  */
/*                                                                         */
/*              GLOBLOAD [-z] [-t] [input-file-spec]                       */
/*                                                                         */
/* The lines of the file must be in form output by a "global *" cmd:       */
/*                      name = value                                       */
/* The variable "name" is created (if necessary) in the global environment */
/* and then assigned the string "value," which runs to the end of the line.*/
/*                                                                         */
/* The -z switch tells the program, after seeing eof, to call DOSSLEEP to  */
/* sleep indefinitely.  If called from the command line, the sleeping      */
/* program could be cancelled with ^Break.  However, the -z option would   */
/* ordinarily be used from the config.sys file, like this:                 */
/*              run=\globload.exe -z file-of-initial-vars                  */
/* Having the program asleep, detached, forever is the means of keeping    */
/* the Global Environment DLL code in storage; otherwise it goes away and  */
/* and the loaded definitions are lost.                                    */
/*                                                                         */
/* Incidentally, the "run=" use is the reason the program takes a named    */
/* file instead of using stdin: there's no support for <redirection for    */
/* programs being run from the config.sys file.                            */
/*                                                                         */
/* The -t switch asks the program to generate and define the value         */
/*              time_of_loading = dd-mm-yy,hh:mm:ss                        */
/* The -t switch may be used in addition to or instead of a filespec.      */
/*                                                                         */
/* Revision History:                                                       */
/*   10/08/87 -- first version completed                                   */
/*   10/12/87 -- commentary, explain(), -t option                          */
/*                                                                         */
/* Copyright (C) 1987 David E. Cortesi                                     */
/*=========================================================================*/
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#define ZIP '\0'
#define EOL '\n'

/*=================================================================*/
/* The following functions are defined by OS/2.                    */
/*=================================================================*/
extern void far pascal DOSSLEEP(long time);
struct DosDateTime {
        unsigned char hour, minute, second, hundredth, day, month;
        unsigned int year, timezone;
        unsigned char dayofweek;
                };
extern void far pascal DOSGETDATETIME(struct DosDateTime far *);

/*=================================================================*/
/* The following functions are defined in the GlobEnv dynalink.    */
/*=================================================================*/
extern int far pascal GLBENTER(char far * thename);
extern int far pascal GLBSTORE(char far * thename
                              ,void far * thevalue
                              ,int thelen);

/*=================================================================*/
/* Explain program use, called on any parameter error.  Exits.     */
/*=================================================================*/
static void explain()
{
        printf(
"\t\tGLOBLOAD  [-z] [-t] filespec\n"
        ); printf(
"Loads the global environment from an ascii file 'filespec.'\n"
        ); printf(
"File lines are in the format output by the GLOBAL command:\n"
        ); printf(
"\tname = [value-string]\n"
        ); printf(
"The -t option make the program insert insert the time and date as:\n"
        ); printf(
"\ttime_of_load = dd-mm-yy,hh:mm:ss\n"
        ); printf(
"using the system time and date.\n"
        ); printf(
"The -z option makes the program sleep forever, which keeps the\n"
        ); printf(
"global-environment DLL code in storage.  Use in the config.sys file:\n"
        ); printf(
"\trun=\\globload.exe -z file-of-initial-vars\n"
        );
        exit(1);
}

/*=================================================================*/
/* The following statics are used in calling the GlobEnv functions */
/*=================================================================*/
#define MAXSTR 120 /* names could get this long on cmd line */
static char xname[MAXSTR];
#define MAXVAL 512      /* arbitrary limit on value size */
static char xvalue[MAXVAL];
static int xsize;
#define MAXLINE MAXSTR+MAXVAL+3
static char inline[MAXLINE];

/*=================================================================*/
/* Build the name time_of_load in xname and the value of the date  */
/* and time in xvalue.                                             */
/*=================================================================*/
static void makevar()
{
struct DosDateTime dt;
        strcpy(xname, "time_of_load");
        DOSGETDATETIME(&dt);
        sprintf(xvalue,"%02d-%02d-%02d,%02d:%02d:%02d"
                ,dt.day ,dt.month ,dt.year-1900
                ,dt.hour ,dt.minute ,dt.second);
        xsize = /* dd-mm-yy,hh:mm:ss\0 */ 18;
}

/*=================================================================*/
/* Process one line from inline into xname, xvalue.  Allow some    */
/* freedom of syntax: spaces before the name and around the = sign */
/* are ok but not required.  Extra stuff between name and = is     */
/* ignored.  Missing value is ok, it becomes null.  Returns zero   */
/* when there is no name (all blank line); otherwise returns 1.    */
/*=================================================================*/
static int parseline()
{
register char *j, *k;
        k = xname;
        for (j=inline; *j == ' '; ++j);
        for (; (*j != EOL)&&(*j != ' ')&&(*j != '='); ) *k++ = *j++;
        *k = ZIP;
        if (k==xname) return(0);
        for (; *j != '=';++j) if (*j==EOL) break;
        k = xvalue;
        if (*j != EOL)
        {
                ++j; /* over the equals sign */
                for(; *j == ' '; ++j) if (*j==EOL) break;
                for(; *j != EOL ;) *k++ = *j++;
        }
        *k = ZIP;
        xsize = (k==xvalue)?0:1+k-xvalue;
        return(1);
}

/*=================================================================*/
/* Assign the value xvalue to the name xname.                      */
/*=================================================================*/
static void assign()
{
int rc;
        rc = GLBENTER(xname);
        if ((rc==0)||(rc==2))
                GLBSTORE(xname,xvalue,xsize);
}

main(argc,argv)
int argc; char *argv[];
{
FILE *inp;
char *parm;
int zparm, nparm;

        /* at least one parameter is required */
        if (argc < 2) explain();

        for (zparm = 0, nparm = 1; nparm < argc; ++nparm)
        {
                if (0 == strcmpi("-z",argv[nparm]))
                        zparm = 1;
                else if (0 == strcmpi("-t",argv[nparm]))
                {       makevar(); assign(); }
                else /* presume it's a file */
                {
                        inp = fopen(argv[nparm],"r");
                        if (NULL != inp)
                                while (NULL != fgets(inline,MAXLINE-1,inp))
                                {
                                        inline[MAXLINE-1] = EOL;
                                        if (parseline())
                                                assign();
                                }
                        else
                        {
                                fprintf(stderr, "Unable to open %s\n",
                                        argv[nparm]);
                                exit(4);
                        }
                } /* end else file */
        } /* end for */
        if (zparm)
                DOSSLEEP(-1L);
}
