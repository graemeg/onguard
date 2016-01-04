TurboPower OnGuard for Free Pascal


Table of contents

* Introduction
* Installation
* Support
* Version history

 

==============================================


Introduction
============

OnGuard is a library to create demo versions or leased versions of your 
applications. Create demo versions that are time-limited, feature-limited, 
limited to a certain number of uses, or limited to a certain # of concurrent 
network users.

This is a source-only release of TurboPower OnGuard ported to Free Pascal. It 
includes packages for use with Lazarus IDE, but Lazarus is not a requirement to
use OnGuard.


Installation
============

To install OnGuard into your IDE, take the following steps:

  1. Unzip the release files into a directory (the best it \components subdirectory 
     of Lazarus tree).

  2. Start Lazarus.

  3. Open & install the package. This requires a Lazarus IDE rebuild.
  
NOTE:
  It is NOT necessary to install OnGuard into Lazarus IDE's component palette
  just to use OnGuard. You can simply add the OnGuard source path to your
  project and instantiated objects via code too. The benefit of this method
  is that it works with any IDE or programmer editor too.


Support
=======

A dedicated support newsgroup has been set up for the Free Pascal port of 
OnGuard. Connection details are as follows:

  NNTP Server: geldenhuys.co.uk
  Port:        119
  Group:       onguard.talk

Any News Client (eg: Mozilla Thunderbird, XanaNews, Opera Mail etc) can be used 
to connect to the news group. This is by far the best option and gives you the 
freedom to use your preferred news client software.

In a pinch, there is also a HTML webnews interface. This interface has some 
limitations (eg: attachments), but is good enough to read and reply to messages
when on the go via a web browser (smartphone or desktop). To access the HTML 
interface, visit the following URL: [http://geldenhuys.co.uk/webnews/]


Version history
===============

* Version history is not maintained here any more. Please review the commit
  log of the source code repository to find detailed version information.

* July 27, 2009
    Initial addition of features:
     - InvalidCount which allow limited count of valid code even if trial period is expired
       (useful due to user mistakes causing incorect system date sometimes)
     - instead of logical volume serial number , hardware hdd ide disk serial number is no used if available
     - compilation with newest Lazarus SVN problems fixed 

* July 28 2009
    - initial support for InvalidCount value allowing 'breaking the rules' limited , for example run program a few times if trial period expired
   - IDE serial number used for machine id instead of volume serials
   - fixed TextHeight obsolete properties in lfm resources

* October 31, 2008

    Import into lazarus-ccr  SVN repository.


* June 4 , 2006

    Conversion to Free Pascal / Lazarus
    -------------------------------------------------------------
    Contributor : Boguslaw Brandys
    Based on Kylix version by Andrew Haines


                       ----------[ end ]-----------
