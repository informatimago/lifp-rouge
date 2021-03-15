#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <signal.h>
#include <sys/types.h>
#include <winsock.h>

#define PORTNUM 8889
#define NETWORK_ERROR -1
#define NETWORK_OK     0

void ReportError(int, const char *);

int stcmp (char* a, char* b)
{int i,n=strlen(a);
  if (n!=strlen(b)) return 1;
  for (i=0;i<n;i++)
    if (a[i]!=b[i]) return 1;
  return 0;
}

int toi (char* str)
{
  if (str[0]==':')
    {
      if (stcmp(str,":LINES")==0) return LINES;
      if (stcmp(str,":COLS")==0) return COLS;
      if (stcmp(str,":ACS_HLINE")==0) return ACS_HLINE;
      if (stcmp(str,":ACS_VLINE")==0) return ACS_VLINE;
      if (stcmp(str,":ACS_URCORNER")==0) return ACS_URCORNER;
      if (stcmp(str,":ACS_LLCORNER")==0) return ACS_LLCORNER;
      if (stcmp(str,":ACS_LRCORNER")==0) return ACS_LRCORNER;
      if (stcmp(str,":ACS_ULCORNER")==0) return ACS_ULCORNER;
      if (stcmp(str,":ACS_TTEE")==0) return ACS_TTEE;
      if (stcmp(str,":ACS_BTEE")==0) return ACS_BTEE;
      if (stcmp(str,":ACS_LTEE")==0) return ACS_LTEE;
      if (stcmp(str,":ACS_RTEE")==0) return ACS_RTEE;
      if (stcmp(str,":ACS_PLUS")==0) return ACS_PLUS;
      if (stcmp(str,":A_BOLD")==0) return A_BOLD;
      if (stcmp(str,":A_UNDERLINE")==0) return A_UNDERLINE;
      if (stcmp(str,":A_REVERSE")==0) return A_REVERSE;
      //colors
      if (stcmp(str,":CBLACK")==0) return COLOR_PAIR(COLOR_BLACK)+0*A_BOLD;
      if (stcmp(str,":CBLUE")==0) return COLOR_PAIR(COLOR_BLUE)+0*A_BOLD;
      if (stcmp(str,":CDGREEN")==0) return COLOR_PAIR(COLOR_GREEN)+0*A_BOLD;
      if (stcmp(str,":CCYAN")==0) return COLOR_PAIR(COLOR_CYAN)+0*A_BOLD;
      if (stcmp(str,":CRED")==0) return COLOR_PAIR(COLOR_RED)+0*A_BOLD;
      if (stcmp(str,":CPURPLE")==0) return COLOR_PAIR(COLOR_MAGENTA)+0*A_BOLD;
      if (stcmp(str,":CBROWN")==0) return COLOR_PAIR(COLOR_YELLOW)+0*A_BOLD;
      if (stcmp(str,":CGRAY")==0) return COLOR_PAIR(COLOR_WHITE)+0*A_BOLD;
      if (stcmp(str,":CDARK")==0) return COLOR_PAIR(COLOR_BLACK)+1*A_BOLD;
      if (stcmp(str,":CLBLUE")==0) return COLOR_PAIR(COLOR_BLUE)+1*A_BOLD;
      if (stcmp(str,":CGREEN")==0) return COLOR_PAIR(COLOR_GREEN)+1*A_BOLD;
      if (stcmp(str,":CSKY")==0) return COLOR_PAIR(COLOR_CYAN)+1*A_BOLD;
      if (stcmp(str,":CROSE")==0) return COLOR_PAIR(COLOR_RED)+1*A_BOLD;
      if (stcmp(str,":CPINK")==0) return COLOR_PAIR(COLOR_MAGENTA)+1*A_BOLD;
      if (stcmp(str,":CYELLOW")==0) return COLOR_PAIR(COLOR_YELLOW)+1*A_BOLD;
      if (stcmp(str,":CWHITE")==0) return COLOR_PAIR(COLOR_WHITE)+1*A_BOLD;

    }
  else return atoi(str);
}     


int execute(char func[256], char args[16][256], int n_arg, SOCKET out)
{int i; int x,y;
  //mvprintw(23,1,"[%s]\n",func);
  //for (i=0;i<n_arg;i++) printf("(%s)\n",args[i]);
  //if (n_arg>=2) mvprintw(24,1,"[%i]",strlen(args[2])); refresh();
  if (stcmp(func,"QUIT")==0) return -1;
  if (stcmp(func,"ERASE")==0) {erase(); return 0;}
  if (stcmp(func,"REFRESH")==0) {refresh();return 0;}
  if (stcmp(func,"MOVE")==0)
    {move(toi(args[0]),toi(args[1]));return 0;}
  if (stcmp(func,"ADDCH")==0)
    {addch(toi(args[0]));return 0;}
  if (stcmp(func,"ADDSTR")==0)
    {addstr(args[0]);return 0;}
  if (stcmp(func,"ATTRON")==0)
    {attron(toi(args[0]));return 0;}
  if (stcmp(func,"ATTROFF")==0)
    {attroff(toi(args[0]));return 0;}
  if (stcmp(func,"GETCH")==0)
    {return 1;}
  if (stcmp(func,"GETYX")==0)
    {getyx(stdscr,y,x);
      char buffer[256];
      sprintf(buffer, "(%d %d)", y, x);
      sendmessage(buffer,out);            
      return 0;
    }
  if (stcmp(func,"ATTRSET")==0)
    {attrset(toi(args[0]));return 0;}
  if (stcmp(func,"MVADDCH")==0)
    {mvaddch(toi(args[0]),toi(args[1]),toi(args[2]));return 0;}
  if (stcmp(func,"MVADDSTR")==0)
    {mvaddstr (toi(args[0]),toi(args[1]),args[2]);return 0;}
  if (stcmp (func,"MVGETCH")==0)
    {move(toi(args[0]),toi(args[1]));return 1;}
  if (stcmp (func,"PRINTW")==0)
    {printw(args[0]);return 0;}
  if (stcmp (func,"MVPRINTW")==0)
    {mvprintw(toi(args[0]),toi(args[1]),args[2]); return 0;}
  return 0;
}


int parsebuffer (char* buffer, SOCKET out)
{int i=0,res=0,n_arg=0,pos=0,spc,quoted=0,escaped=0; 
 char c; char func[256]; char arg[16][256];
 char* curstr=func;
 //mvprintw(24,1,"%s",buffer);   
    while (i<strlen(buffer))
    {n_arg=0;pos=0;quoted=0;escaped=0;curstr=func;
     c=buffer[i];
     if (c!='(') return 0;
     spc=1;
     while ((c!=')')||escaped||quoted)
        {i++;c=buffer[i];
         switch(c) 
         {
         case ' ':
            if (!escaped&&!quoted)
             {if (spc)
               {spc=0;
                curstr[pos]=0; pos=0;
                if (curstr==func) curstr=arg[0]; else {n_arg++;curstr=arg[n_arg];}
               }
              }
             else {spc=1;curstr[pos]=c;pos++;escaped=0;};  
            break;
         case '"':
            if (!escaped) quoted=!quoted;
            else {spc=1;curstr[pos]=c;pos++;escaped=0;}
            break;
         case '\\':
            if (!escaped) escaped=1; else {spc=1;curstr[pos]=c;pos++;escaped=0;}
            break;
         default:
         spc=1;curstr[pos]=c;pos++;escaped=0;}         
         }
    curstr[pos-1]=0; n_arg++;i++;
    res=execute(func, arg, n_arg,out);
    }
    return res;
}
         
         


static void finish(int sig)
{
    endwin();

    /* do your non-curses wrapup here */

    exit(0);
}

int sendmessage (char* buffer, SOCKET out)
{int n=strlen(buffer);
    buffer[n]=' ';
    buffer[n+1]=0;
    send(out,buffer,strlen(buffer),0);
}
            
    
int main(int argc, char *argv[])
{ int i,c;

//printf("Type zzz to continue...");
//Weird curses stuff....
    (void) signal(SIGINT, finish);      /* arrange interrupts to terminate */

    (void) initscr();      /* initialize the curses library */
    keypad(stdscr, TRUE);  /* enable keyboard mapping */
    (void) nonl();         /* tell curses not to do NL->CR/NL on output */
    (void) cbreak();       /* take input chars one at a time, no wait for \n */
    (void) noecho();       /* don't echo input */
    intrflush(stdscr, FALSE);

        init_pair(COLOR_BLACK, COLOR_BLACK, COLOR_BLACK);
        init_pair(COLOR_GREEN, COLOR_GREEN, COLOR_BLACK);
        init_pair(COLOR_RED, COLOR_RED, COLOR_BLACK);
        init_pair(COLOR_CYAN, COLOR_CYAN, COLOR_BLACK);
        init_pair(COLOR_WHITE, COLOR_WHITE, COLOR_BLACK);
        init_pair(COLOR_MAGENTA, COLOR_MAGENTA, COLOR_BLACK);
        init_pair(COLOR_BLUE, COLOR_BLUE, COLOR_BLACK);
        init_pair(COLOR_YELLOW, COLOR_YELLOW, COLOR_BLACK);

//end
//  c1=getch()-'z'; c2=getch()-'z'; c3=getch()-'z';
//  keydiff=0;
//  attrset(COLOR_PAIR(COLOR_YELLOW));
//  mvprintw(24,1, "FUCK YOU!!!!");
//  refresh();
//  c=getch();
//        attrset(4);


	WORD sockVersion;
	WSADATA wsaData;
	int nret,needinput;

	sockVersion = MAKEWORD(1, 1);			// We'd like Winsock version 1.1


	// We begin by initializing Winsock
	WSAStartup(sockVersion, &wsaData);


	// Next, create the listening socket
	SOCKET listeningSocket;

	listeningSocket = socket(AF_INET,		// Go over TCP/IP
			         SOCK_STREAM,   	// This is a stream-oriented socket
				 IPPROTO_TCP);		// Use TCP rather than UDP

	if (listeningSocket == INVALID_SOCKET) {
		nret = WSAGetLastError();		// Get a more detailed error
		ReportError(nret, "socket()");		// Report the error with our custom function

		WSACleanup();				// Shutdown Winsock
		return NETWORK_ERROR;			// Return an error value
	}


	// Use a SOCKADDR_IN struct to fill in address information
	SOCKADDR_IN serverInfo;

	serverInfo.sin_family = AF_INET;
	serverInfo.sin_addr.s_addr = INADDR_ANY;	// Since this socket is listening for connections,
							// any local address will do
	serverInfo.sin_port = htons(PORTNUM);		// Convert integer 8888 to network-byte order
							// and insert into the port field


	// Bind the socket to our local server address
	nret = bind(listeningSocket, (LPSOCKADDR)&serverInfo, sizeof(struct sockaddr));

	if (nret == SOCKET_ERROR) {
		nret = WSAGetLastError();
		ReportError(nret, "bind()");

		WSACleanup();
		return NETWORK_ERROR;
	}


	// Make the socket listen
	nret = listen(listeningSocket, 10);		// Up to 10 connections may wait at any
							// one time to be accept()'ed

	if (nret == SOCKET_ERROR) {
		nret = WSAGetLastError();
		ReportError(nret, "listen()");

		WSACleanup();
		return NETWORK_ERROR;
	}


	// Wait for a client
	SOCKET theClient;

	theClient = accept(listeningSocket,
			   NULL,			// Address of a sockaddr structure (see explanation below)
			   NULL);			// Address of a variable containing size of sockaddr struct

	if (theClient == INVALID_SOCKET) {
		nret = WSAGetLastError();
		ReportError(nret, "accept()");

		WSACleanup();
		return NETWORK_ERROR;
	}
	
	while (needinput!=-1) {
	char buffer[256];
    for (i=0;i<256;i++) buffer[i]=0;
	nret=recv(theClient,
	    buffer,
	    256,		// Complete size of buffer
	    0);
	needinput=parsebuffer(buffer,theClient); 
        if (needinput==1) 
          {c=getch(); itoa(c,buffer,10);
            mvprintw(24,1,"[%s]\n",buffer);            
            sendmessage(buffer,theClient);
            needinput=0;
            //printf("OK\n");
            }
        
          
        //printf("%s\n",buffer);
    }
	// Send and receive from the client, and finally,
	closesocket(theClient);
	closesocket(listeningSocket);


	// Shutdown Winsock
	WSACleanup();



  system("PAUSE");	
  finish(0);
  return 0;
}

void ReportError(int errorCode, const char *whichFunc) {
   char errorMsg[92];					// Declare a buffer to hold
							// the generated error message
   
   ZeroMemory(errorMsg, 92);				// Automatically NULL-terminate the string

   // The following line copies the phrase, whichFunc string, and integer errorCode into the buffer
   sprintf(errorMsg, "Call to %s returned error %d!", (char *)whichFunc, errorCode);

   MessageBox(NULL, errorMsg, "socketIndication", MB_OK);
}


