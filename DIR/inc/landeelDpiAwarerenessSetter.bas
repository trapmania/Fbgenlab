#include once  "windows.bi"
   'credit::landeel@fb.net
   
   '
   dim shared shcoredll as any ptr
   #define PROCESS_PER_MONITOR_DPI_AWARE 2
   '
   dim shared SetProcessDpiAwareness as function cdecl(byval PROCESS_DPI_AWARENESS as long=PROCESS_PER_MONITOR_DPI_AWARE) as long
   dim ret1 as long
   shcoredll=dylibload("shcore")
   if shcoredll=0 then
      print "DPI : Error loading shcore.dll"
      end 1
   else
      print "DPI : loaded shcore.dll"
   end if
   '
   SetProcessDpiAwareness=DyLibSymbol(shcoredll,"SetProcessDpiAwareness")
   if SetProcessDpiAwareness=0 then
      print "DPI : SetProcessDpiAwareness not found."
      end 2
   else
      print "DPI : found SetProcessDpiAwareness"
   end if
   ret1=SetProcessDpiAwareness(PROCESS_PER_MONITOR_DPI_AWARE)
   if ret1=S_OK then
      print "DPI : SetProcessDpiAwareness() returned "+str(ret1)+" : success!" 'S_OK (0) = success
   else
      print "DPI : SetProcessDpiAwareness() returned "+str(ret1)+" : failure!" 'failure
   end if

'(eof)