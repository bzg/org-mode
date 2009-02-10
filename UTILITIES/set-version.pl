#!/usr/bin/perl
$version = $ARGV[0];
if ($version eq "--all" or $version eq "-a") {
  $all = 1;
  $version = $ARGV[1]
}

die "No version given" unless $version=~/\S/;
$date = `date "+%B %Y"`; chomp $date;
$year = `date "+%Y"` ; chomp $year;

print STDERR "Changing version to \"$version\" and date to \"$date\" in all relevant files\n" ;

print STDERR join("\n",glob("lisp/*.el")),"\n";
$cmd = qq{s/^(;; Version:)\\s+(\\S+)[ \t]*\$/\$1 $version/;s/^(\\(defconst org-version )"(\\S+)"/\$1"$version"/};
$c1 = "perl -pi -e '$cmd' lisp/*.el";
system($c1);

print STDERR "doc/org.texi\n";
$cmd = qq{s/^(\\\@set VERSION)\\s+(\\S+)[ \t]*\$/\$1 $version/;s/^(\\\@set DATE)\\s+(.*)\$/\$1 $date/;};
$c1 = "perl -pi -e '$cmd' doc/org.texi";
system($c1);

print STDERR "doc/orgcard.tex\n";
$cmd = qq{s/^\\\\def\\\\orgversionnumber\\{\\S+\\}/\\\\def\\\\orgversionnumber{$version}/;s/\\\\def\\\\versionyear\\{\\S+\\}/\\\\def\\\\versionyear{$year}/;s/\\\\def\\\\year\\{\\S+\\}/\\\\def\\\\year{$year}/;};
$c1 = "perl -pi -e '$cmd' doc/orgcard.tex";
system($c1);

print STDERR "README_DIST\n";
$cmd = qq{s/^(The version of this release is:)\\s+(\\S+)[ \t]*\$/\$1 $version/;};
$c1 = "perl -pi -e '$cmd' README_DIST";
system($c1);

if ($all) {
  print STDERR "ORGWEBPAGE/index.org\n";
  $cmd = qq{s/^(\\* Current Version )\\(\\S+?\\)/\$1($version)/;s/^(The current version is)\\s+(\\S+)\\. /\$1 $version. /;s/org-[6-9].*?\\.(zip|tar\\.gz)/org-$version.\$1/g};
  $c1 = "perl -pi -e '$cmd' ORGWEBPAGE/index.org";
  system($c1);
}
