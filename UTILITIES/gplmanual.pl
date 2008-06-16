#!/usr/bin/perl

# This command creates a version of the manual to is both GFDL and GPL
# To allow DEBIAN to include the manual in its main section.

$gpl = 
'

Permission is also granted to copy, distribute and/or modify this document
under the terms of the GNU General Public License (GPL).
You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see @url{http://www.gnu.org/licenses/}.
';

open IN, "<doc/org.texi" or die "Cannot open doc/org.texi\n";
open OUT, ">doc/org_dual_license.texi" or die "Cannot open doc/org.texi\n";

while (<IN>) {
  print OUT $gpl if /end quotation/;
  print OUT $_;
 }
