Development Enviroment:
=======================

Embarcadero Delphi XE2:
http://www.embarcadero.com/products/delphi/


Used packages:
==============

FastMM:
https://github.com/pleriche/FastMM4/

TPerlRegEx:
http://www.regular-expressions.info/delphi.html

Synopse PDF:
http://synopse.info/fossil/wiki?name=PDF+Engine

Unicode SynEdit:
https://github.com/SynEdit/SynEdit/

Active Query Builder VCL Edition:
http://www.activequerybuilder.com/

ShellBrowser:
http://www.jam-software.de/shellbrowser_delphi/

Mirkes MPHexEditor:
https://launchpad.net/dcr/

Note: MPHexEditor was developed by Markus Stephany, who stopped his work.
Later on the Delphi Code Revival project worked on it - but seems not
very long. Inside this project there is an own transformation from Delphi 7
to Delphi XE2.


Tools to build the setup program:
=================================

EurekaLog:
http://www.eurekalog.com/

Gawk for Windows:
http://gnuwin32.sourceforge.net/packages/gawk.htm

Help & Manual:
http://www.ec-software.com/

HTML Help Workshop:
http://go.microsoft.com/fwlink/?LinkId=14188

Inno Setup:
http://www.jrsoftware.org/isinfo.php


Developing the program:
=======================

Inside Delphi first of all, you have to download all packages from the given
URLs. Store the source code into a new folder "Source" inside the file system.
A new folder called "Resource" need the .res and .dfm files. After this, you 
have to built all packages inside Delphi (Menu: Project -> Build All Project).

All Designtime packages needs to be installed. This will be done inside the
IDE with the project manager (Menu: View -> Project Manager). For each
package, you have to make a right click -> Install.

To get detailed memory leak informations while debugging, you have to store
FastMM_FullDebugMode.dll inside the Temp folder.

Build a setup program. This will generate the resource file with the icons,
strings, version information and others.


Building the setup program:
===========================

The setup program will be built with the Build_Setup.bat file.

With Clean.bat all temporary files can be deleted easily.