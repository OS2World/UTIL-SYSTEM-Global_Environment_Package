/*=========================================================================*/
/*                                                                         */
/*              GLOB2SET : Get Global Names as Shell SET Commands          */
/*                                                                         */
/*                      glob2set [xyz]*                                    */
/*                                                                         */
/* The Global Environment for OS/2 is a store of named values that is      */
/* maintained by dynalink code defined in GLOBENV.ASM/.OBJ/.DLL/.LIB.      */
/* There's no communication between the Global Environment and the command */
/* environment maintained by the command shell.  Moreover, there's no way  */
/* by which a program that is called as a command may make permanent       */
/* insertions in the environment it inherits.  Therefore we cannot write   */
/* a program to copy strings from the global environment to the local one. */
/*                                                                         */
/* What this program does is to copy a selected set of global variables    */
/* to stdout in the format of a series of SET commands.  If the output     */
/* is captured in a command file and executed, the transfer from global    */
/* to local environments will have been made, e.g. do:                     */
/*                                                                         */
/*      glob2set * >setall.cmd && setall                                   */
/*                                                                         */
/* The first parameter is a prefix string which may end in an asterisk     */
/* (similar to the GLOBAL command, q.v.).  Only variables whose names      */
/* start with that prefix will be copied.  An asterisk alone copies all    */
/* the variables.                                                          */
/*                                                                         */
/* Two problems remain: the names often copied into the local environment  */
/* have heterogenous names that don't lend themselves to being copied in a */
/* group (e.g. LIB, PATH).  And the Global environment will likely be used */
/* for other items that might have conflicting names.  We solve these at a */
/* stroke with the following KLUDGE RULE:                                 */
/*   If the search prefix string ends in underscore, the entire prefix     */
/* will be stripped from the output names.  Thus the names ZZ_path, ZZ_lib */
/* and ZZ_dpath may be copied out as path, lib and dpath with one command, */
/*      glob2set ZZ_* >set3.cmd                                            */
/*                                                                         */
/* Revision History:                                                       */
/*   10/08/87 -- first version completed                                   */
/*   10/12/87 -- commentary, explain()                                     */
/*                                                                         */
/* Copyright (C) 1987 David E. Cortesi                                     */
/*=========================================================================*/
#include <ctype.h>
#include <stdio.h>
#include <string.h>
/*=================================================================*/
/* The following functions are defined in the GlobEnv dynalink.    */
/*=================================================================*/
extern int far pascal GLBQUERY(char far * thename
                              ,int far * thesize);
extern int far pascal GLBFETCH( char far * thename
                              , void far * valbuff
                              , int far * thesize);
extern int far pascal GLBNEXT ( char far * thename
                              , int strmax
                              , char far * valbuff
                              , int far * thesize);

/*=================================================================*/
/* Explain function called on any parameter error.  Exits.         */
/*=================================================================*/
static void explain()
{
        printf(
"\t\tglob2set  [pfx]*\n"
        ); printf(
"Copies Global Environment variables to stdout as SET commands.\n"
        ); printf(
"To copy variables from Global to command-line environment, redirect\n"
        ); printf(
"output to a .cmd file and then execute, e.g.\n"
        ); printf(
"\tglob2set loc_* >tempset.cmd && tempset\n"
        ); printf(
"Names beginning with prefix 'pfx' are copied; use * alone to copy all.\n"
        ); printf(
"Kludge rule: if 'pfx' ends in an underscore as in example, it is\n"
        ); printf(
" stripped from each set-name, allowing subsetting of global vars.\n"
        );
}

/*=================================================================*/
/* The maximum command line is 127 bytes (plus a nul).  Of that we */
/* need 5 for SET, space, and the equals sign, leaving 122.  Since */
/* a name has to be at least 1 byte and a value ditto, the max     */
/* size for either is 121, to leave room for the other.            */
/*=================================================================*/
#define MAXSTR 121+1
static char xname[MAXSTR];
#define MAXVAL 121+1
static char xvalue[MAXVAL];
static int valsize;     /* value size word parameter space */
static char outline[128];

/*=================================================================*/
/* getlist(int strip) gets the value of xname and writes name and  */
/* value as a set command to stdout.  The parameter strip is the   */
/* number of bytes to strip from the name under the Kludge Rule.   */
/*=================================================================*/
static void getlist(int strip)
{
        if (valsize > MAXVAL)
        {
                fprintf(stderr,"value of %s is %d bytes, only %d copied\n"
                                     ,xname ,valsize      ,MAXVAL);
                valsize = MAXVAL;
        }
        GLBFETCH(xname,xvalue,&valsize);
        /* ensure a string delimiter on a value of max length */
        if (valsize == MAXVAL)
                xvalue[MAXVAL-1] = '\0';
        /* convert a null value to a single space, otherwise the */
        /* shell will take "name=" as a delete request. */
        if (valsize == 0)
                {xvalue[0] = ' '; xvalue[1] = '\0'; }
        printf("SET %s=%s\n",xname+strip,xvalue);
}

/*=================================================================*/
/* listall(char *pfx) does the search for matching names.          */
/*=================================================================*/
static void listall(char *pfx)
{
int len, strip;
char x, yname[MAXSTR];

        strcpy(xname,pfx);
        len = strlen(xname);
        if (xname[len-1]=='*')  /* if a trailing star, zot it */
                xname[--len] = '\0';
        strcpy(yname,xname);  /* save prefix for compares */
        strip = (yname[len-1]=='_')?len:0;
        /*=========================================================*/
        /* GLBNEXT only returns the "next," so if there's a match  */
        /* to the given name we have to get it first.  For an      */
        /* exact match it's obviously wrong to strip a prefix.     */
        /*=========================================================*/
        if (0==GLBQUERY(xname,&valsize))
                getlist(0);
        /*=========================================================*/
        /* Now we can spin through all the "nexts" there may be.   */
        /*=========================================================*/
        while (0==GLBNEXT(xname,MAXSTR,xname,&valsize))
        {
                x = xname[len];         /* temporarily truncate to */
                xname[len] = '\0';      /* length of prefix */
                if (0 != strcmp(xname,yname)) break;
                xname[len] = x;         /* still equals prefix, so */
                getlist(strip);         /* output it.       */
        }
}

int  main(argc,argv)
int  argc;
char *argv[];
{
        if ((argc < 2)||(argc > 3)) explain();
        else listall(argv[1]);
}
