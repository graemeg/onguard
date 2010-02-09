TurboPower OnGuard Free Pascal/Lazarus version


Table of contents

1.  Introduction
2.  Package names
3.  Installation
4.  Version history
4.1 Release 1.13
 

==============================================


1. Introduction


OnGuard is a library to create demo versions of your Borland Delphi &
Kylix  & Lazarus applications. Create demo versions that are time-limited,
feature-limited, limited to a certain number of uses, or limited to a
certain # of concurrent network users.

This is a source-only release of TurboPower OnGuard ported to Lazarus. It includes
package for Lazarus 0.9.16 official installer (and should run fine on later versions too)

==============================================

2. Installation


To install TurboPower OnGuard into your IDE, take the following steps:

  1. Unzip the release files into a directory (the best it \components subdirectory of Lazarus tree).

  2. Start Lazarus.

  3. Open & install the package.This require  Lazarus IDE rebuild.

==============================================

4. Version history

4.3 July 27, 2009
    Initial addition of features:
     - InvalidCount which allow limited count of valid code even if trial period is expired
       (useful due to user mistakes causing incorect system date sometimes)
     - instead of logical volume serial number , hardware hdd ide disk serial number is no used if available
     - compilation with newest Lazarus SVN problems fixed 

4.3 July 28 2009
    - initial support for InvalidCount value allowing 'breaking the rules' limited , for example run program a few times if trial period expired
   - IDE serial number used for machine id instead of volume serials
   - fixed TextHeight obsolete properties in lfm resources

4.2 October 31, 2008

    Import into lazarus-ccr  SVN repository.


4.1 June 4 , 2006

    Conversion to Free Pascal / Lazarus
    -------------------------------------------------------------
    Contributor : Boguslaw Brandys
    Based on Kylix version by Andrew Haines


TODO :
   - compilation under Linux + fixes (implement LockFile functions)
   - testing