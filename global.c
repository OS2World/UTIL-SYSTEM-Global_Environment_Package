/*=========================================================================*/
/*                                                                         */
/*              GLOBAL  :  Command access to Global Environment            */
/*                                                                         */
/* The Global Environment for OS/2 is a store of named values that is      */
/* maintained by dynalink code defined in GLOBENV.ASM/.OBJ/.DLL/.LIB.      */
/* This command makes the contents of the Global Environment accessible    */
/* from the OS/2 protect-mode command line.  See also GLOBLOAD, GLOB2SET.  */
/*                                                                         */
/* Here are the forms and uses of the GLOBAL command:                      */
/*                                                                         */
/*              GLOBAL *                                                   */
/*              GLOBAL xyz*                                                */
/*              GLOBAL name                                                */
/*                                                                         */
/* The first form lists all names and their values.                        */
/* The second lists names and values of variables whose names start "xyz." */
/* The third lists the name and value of variable "name."                  */
/* All three list to stdout, one line per name, in the form:               */
/*              namestring = valuestring                                   */
/* There is no guarantee, of course, that valuestring is printable, but    */
/* it usually will be.  Use redirection to save values in a file, e.g. for */
/* reloading with GLOBLOAD.                                                */
/*                                                                         */
/*              GLOBAL name =                                              */
/*              GLOBAL name = value                                        */
/*                                                                         */
/* These forms define "name" if it isn't defined.  The first assigns a     */
/* null value to name and the second an explicit value.  Use quotes to     */
/* protect spaces and special characters in the value token.               */
/*                                                                         */
/*              GLOBAL name #                                              */
/*                                                                         */
/* This form deletes the variable "name" from the global environment.      */
/*                                                                         */
/* NOTE: if there exists a name longer than MAXSTR, this command can't     */
/* list it, furthermore no name following that name will appear in the     */
/* list produced by "global *"                                             */
/*                                                                         */
/* Revision History:                                                       */
/*   10/06/87 -- first version completed                                   */
/*   10/08/87 small bugs fixed, added # for delete                         */
/*   10/13/87 restriction noted                                            */
/*                                                                         */
/* Copyright (C) 1987 David E. Cortesi                                     */
/*=========================================================================*/
#include <ctype.h>
#include <stdio.h>
#include <string.h>
/*=================================================================*/
/* The following functions are defined in the GlobEnv dynalink.    */
/*=================================================================*/
#include "globenv.h"

/*=================================================================*/
/* The following statics are used in calling the above functions   */
/*=================================================================*/
#define MAXSTR 128 /* names could get this long on cmd line */
static char xname[MAXSTR];
#define MAXVAL 512      /* arbitrary limit on value size */
static char xvalue[MAXVAL];
static int valsize;     /* value size word parameter space */

/*=================================================================*/
/* Explain() function taking usage msg out of line.                */
/*=================================================================*/

static void explain()
{
        printf(
"GLOBAL: command line access to the global environment space.\n"
                ); printf(
"  GLOBAL *                     -- list all names & values   \n"
                ); printf(
"  GLOBAL xyz*                  -- list names beginning 'xyz'\n"
                ); printf(
"(redirect output of above commands to make input for GLOBLOAD)\n"
                ); printf(
"  GLOBAL name                  -- list name, value of 'name'\n"
                ); printf(
"  GLOBAL name =                -- assign null value to 'name'\n"
                ); printf(
"  GLOBAL name = 'value string' -- assign value to 'name'\n"
                ); printf(
"  GLOBAL name #                -- delete the variable 'name'\n"
                );
        exit(0);
}

/*=================================================================*/
/* getlist() gets the value of xname and lists it to stdout.  The  */
/* name is known to exist and valsize is its value size.           */
/*=================================================================*/
static void getlist()
{
        if (valsize > MAXVAL)
        {
                fprintf(stderr,"value of %s, %d, truncated to %d\n"
                                     ,xname ,valsize, MAXVAL);
                valsize = MAXVAL;
        }
        xvalue[0] = '\0';
        GLBFETCH(xname,xvalue,&valsize);
        if (valsize == MAXVAL)
                xvalue[MAXVAL-1] = '\0'; /* ensure string delim */
        printf("%s = %s\n",xname,xvalue);
}

/*=================================================================*/
/* listall(char *) lists all names matching a prefix               */
/*=================================================================*/
static void listall(char *name)
{
int len;
char x, yname[MAXSTR];
        strcpy(xname,name);
        xname[strlen(xname)-1] = '\0'; /* zot the trailing '*' */
        strcpy(yname,xname);    /* save original prefix for cmpr */
        len = strlen(yname);
        /*=========================================================*/
        /* GLBNEXT only returns the "next," so if there's a match  */
        /* to the given name we have to get it first.              */
        /*=========================================================*/
        if (0==GLBQUERY(xname,&valsize))
                getlist();
        /*=========================================================*/
        /* Now we can spin through all "nexts" there are.  NOTE:   */
        /* stops at the first nonzero return code, but that could  */
        /* be 3 for next-name-longer-than-MAXSTR.  In order to get */
        /* past that name we'd have to retrieve it with this call, */
        /* and we can't.                                           */
        /*=========================================================*/
        while (0==GLBNEXT(xname,MAXSTR,xname,&valsize))
        {
                x = xname[len];         /* temporarily truncate to */
                xname[len] = '\0';      /* length of prefix */
                if (0 != strcmp(xname,yname)) break;
                xname[len] = x;         /* still equals prefix */
                getlist();              /* restore and list */
        }
}

/*=================================================================*/
/* listone(char *) lists one name                                  */
/*=================================================================*/
static void listone(char *name)
{
        strcpy(xname,name);
        if (0==GLBQUERY(xname,&valsize))
                getlist();
        else printf("name %s not found\n",xname);
}

/*=================================================================*/
/* assign(char *, char *) assigns a value to a name, defining it   */
/* if it isn't already defined.                                    */
/*=================================================================*/
static void assign(char *name, char *value)
{
int rc;
        strcpy(xname, name);
        GLBENTER(xname); /* ignore return code */
        rc = GLBSTORE(xname,value, 1+strlen(value));
        if (rc)
                fprintf(stderr
                ,"Unable to store value, error code %d",rc);
}

/*=================================================================*/
/* delete(char *) deletes a name from the global environment.      */
/*=================================================================*/
static void delete(char *name)
{
int rc;
        if (rc = GLBDELETE(name))
                fprintf(stderr
                ,"Unable to delete %s, error code %d",name,rc);
}

/*=================================================================*/
/* main() proc: check parameters, call one of above functions      */
/*=================================================================*/
int  main(argc,argv)
int  argc;
char *argv[];
{
        if ((argc == 1)||(argc > 4)) explain();
        if (argc == 2)
        { /* forms with only one parameter */
                if (0==strcmp("?",argv[1])) explain();
                if ('*'==argv[1][strlen(argv[1])-1])
                        listall(argv[1]);
                else
                        listone(argv[1]);
        }
        else
        { /* forms with 2, 3 parameters */
                if (argv[2][1]!='\0') explain();
                else switch (argv[2][0])
                {
                case '=' : assign(argv[1], argv[3]);
                                break;
                case '#' : delete(argv[1]);
                                break;
                default: explain();
                }
        }
}
