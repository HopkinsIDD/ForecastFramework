#R6Class(...-
#classname = ...
#inherit = ...
#public = list(...
# )
#These are used for more legible assignment syntax
use strict;
use warnings;
use File::Find;
use Cwd 'abs_path';
my $path = abs_path($0);
##For documenting use case
#my $mode = 'abstract';
#For documenting all methods
my $mode = 'all';
#print "The arguments passed to this program are: @ARGV\n";

#This is the output file
my $outfilename = 'vignettes/ClassDiagram.Rnw';
my $docfilename = 'tests/documentation-errors.txt';
open(OUT,"> $outfilename") or die $!;
#we want to actually parse all the code files.
my $folder = 'R';
opendir(DIR,$folder);
my @files = readdir(DIR);
#These are weird.  Eventually %classes will be a hash of classes (hash) of lists (hash) of methods (array).  Didn't know what I wanted to pull when I wrote this, so I pulled mostly everything.
my %superclasses;
my %classes;
my $class_counter = 0;
my %is_abstract;
my %is_inherited;
my %documentation;
my %has_return;

open(DOC,"> $docfilename");

my $outer_debug = '';
#Uncomment the next line to enable debug mode.
#When prompted enter 'y' to start debugging.
$outer_debug = <>;
foreach my $file (@files){
  print "FILE $file\n";
  my $debug;
  if($outer_debug =~ "y\n"){
    $debug = <>;
  }
  if($file =~ m/^\.+$/){
    next;
  }
  open(IN,"< $folder/$file") or die "Could not open file $file: $!\n";
  my $in_class = 0;
  my $in_list = 0;
  my $in_method = 0;
  my $in_documentation= 0;
  my $in_multiline_string = 0;
  my $class_name = '';
  my $list_name = '';
  my $method_name = '';
  my $statements_deep = 0;
  my $parens_deep = 0;
  my $string_type = '';
  #The following are references only.
  my $this_class = \{};
  my $this_list = \{};
  my $this_method = \[];
  my $documentation_buffer = '';
  while(my $line = <IN>){
    if(defined($debug) && ($debug eq "y\n")){
      print $line;
      print "\n$class_name $list_name $method_name $in_documentation $in_multiline_string $statements_deep\n";
      my $useless = <>;
    }
    chop $line;
    my @split_text = split('#',$line);
    my $comment_line = '';
    #print "\t0-$split_text[0]\n";
    if(scalar(@split_text) > 1){
      my $tmp = '#' . join('#',@split_text[1..(scalar(@split_text)-1)]);
      #print "\t1-$tmp\n";
      my $comment_line = "$tmp\n";
      #$documentation{$file.$class_name.$list_name.$method_name} .= $comment_line
      $documentation_buffer .= $comment_line;
      #print "$documentation_buffer\n";
    }
    if($split_text[0]){
      $line = "$split_text[0]\n";
    } else{
      $line = "\n";
    }
    #Starting a new file
    if($in_class > 0){
      if($in_list > 0){
        if($in_method > 0){
          if($in_documentation > 0){
            if($in_multiline_string > 0){
              print "In Multiline String\n";
              if($line =~ /$string_type/){
                #Something should happen if there is a string character
                if($line =~ /^\s*(.*)$string_type\s*$/){
                  #If the last character is a string delimiter
                  $in_multiline_string = 0;
                  $documentation{$class_name.$list_name.$method_name} .= "D-#' $1\n";
                } else{
                  die "Something is wrong here\n";
                }
              } else{
                $line =~ /^\s*(.*)\s*$/;
                $documentation{$class_name.$list_name.$method_name} .= "C-#' $1\n";
              }
            } else{
              #If function starts with a bare string, we are in the old documentation style.
              if($line =~ /^\s*(["'])/){
                #Finish reading the string
                $string_type = $1;
                #print "$string_type\n";
                #print "^\s*$string_type([^$string_type]+)$string_type\s*\$\n";
                if($line =~ /^\s*$string_type([^$string_type]+)$string_type\s*$/){
                  $documentation{$class_name.$list_name.$method_name} .= "A-#' $1\n";
                } else{
                  if($line =~ /^\s*$string_type([^$string_type]+)\s*$/){
                    $documentation{$class_name.$list_name.$method_name} .= "B-#' $1";
                    my $finished = 0;
                    $in_multiline_string = 1;
                  } else{
                    die "There is a problem here 1...\n";
                  }
                }
              } else{
                if($line =~ /^\s*$/){
                  #print "Blank line :---$line---:\n";
                } else{
                  $in_documentation = 0;
                }
              }
            }
          }
          
          #Check to see if the method has a return.
          if($line =~ /^\s*return\(\s*[^   ].*\)/){
            $has_return{$class_name.$list_name.$method_name} = 1;
          }
          
          #Check if we are leaving the method
          if($line =~ /^\s*[{}],?\s*$/){
            if($statements_deep == 0){
              $in_method = 0;
              print "\t\t\tEnding $method_name\n";
              $documentation_buffer = '';
            }
          }
          #Check if this is an abstract method (note: any method which has defaultAbstract in it is abstract right now...
          #could include other stuff here, but i'm not
          if($line =~ /^[^#]*defaultAbstract.*$/){
            $is_abstract{$class_name . $list_name . $method_name} = 1;
            $is_abstract{$class_name} = 1;
            print "\t\t\t\t$class_name::$list_name::$method_name is abstract\n";
          }

          if($line =~ /[^#]*}.*/){
            if($in_method > 0){
              $statements_deep -= 1;
            }
          }

          if($line =~ /[^#]{.*/){
            if($in_method > 0){
              $statements_deep += 1;
            }
          }
        } else{
          #Check if list is ending
          if($line =~ /.*[()].*/){
            #There  are some parentheses.
            if($line =~ /.*\(.*/){
              if(($line !~ /.*function.*/) && ($line !~ /.*\).*/)){
                ++$parens_deep;
              }
            }

            if($line =~ /^\s*\),?.*/){
              if($parens_deep == 0){
                $in_list = 0;
                print "\t\tEnding $list_name\n";
              } else{
                --$parens_deep;
              }
            }
          }
          #check if a function is starting (note: for now all members are functions)
          #putting an extra comment line to emphasize the above for when someone adds
          #a public member
          #detect if a function is beginning.
          if($line =~ /^\s*[^=   #]*\s*=\s*function.*$/){
            #If the function is on a single line, we can get all the arguments together...
            #Check for single line function definitions
            my $args = '';
            if($line =~ /^\s*([^=   #]*)\s*=\s*function\s*\((.*)\)\s*\{\s*$/){
              #extract the arguments of the function (captured by the regex above
              #$1 and $2 are variables corresponding to the regex in parens ($1 is the first, and $2 is the second set of parens
              #in this case $1 is the name of the function some non-space non equals characters[^= \t]*
              #in this case $2 is the list of arguments (anything inside the escaped parentheses)
              $args = $2;
              $method_name = $1;
              #print "$documentation{$class_name}\n";
              $documentation{$class_name.$list_name.$method_name} = $documentation_buffer;
              $documentation_buffer = '';
            } else{
              #We have a multiline function...
              if( $line =~/^\s*([^=   #]*)\s*=\s*function\s*\(([^{]*)\s*$/){
                $method_name = $1;
                $documentation{$class_name.$list_name.$method_name} = $documentation_buffer;
                $documentation_buffer = '';
                $args = $2;
                chop $args;
                $line = readline(IN);
                while(
                  $line =~ m/^\s*([^{]*)\s*$/
                ){
                  #print "~~$line matches\n~~";
                  $args = $args . $1;
                  chop $args;
                  #print "--$line--\n";
                  #print "==$args==\n";
                  #my $continue = <>;
                  #if($continue =~ "n\n"){die;}
                  $line = readline(IN);
                }
                #For picking up the last line including any potential arguments on it.
                if($line =~ /^\s*([^{#   ]*)\)\s*\{/){
                  #print $line;
                  $args = $args . $1;
                } else{
                  die "Function didn't finish line:\n$line\n";
                }
              } else{
                die "There is a problem here 2...\n$line\n";
              }
            }
            $args =~ s/\(/(/g;
            $args =~ s/\)/)/g;
            while($args =~ s/(\(.*),(.*\))/$1;$2/g){
              next;
            }
            my @args = split(',',$args);
            #' Go through and make sure no arguments have ',' in them
            $this_list->{$method_name} =[];
            $this_method = \@{$this_list->{$method_name}};
            $in_method = 1;
            $in_documentation = 1;
            print "\t\t\tstarting method $method_name with arguments ";
            foreach my $arg (@args){
              while($arg =~ s/(\(.*);(.*\))/$1,$2/g){
                next;
              }
              print "$arg,";
              push(@{$this_method},$arg);
            }
            print"\n";
          }
        }
      } else{
        #Check to see if we leave the class
        if($line =~ /\s*[)]\s*$/){
          print "\tEnding $class_name\n";
          $in_class = 0;
          $this_class = \{};
        }
        #Check to see if we enter a list
        if($line =~ /^\s*([^=   ]*)\s*=\s*list[(]/){
          #$1 is a variable corresponding to the regex in parens. In this case it matches the name of the list [%= \t]*
          $list_name = $1;
          $this_class->{$list_name} = {};
          $this_list = \%{$this_class->{$list_name}};
          $in_list = 1;
          #print "--$list_name--\n";
          print "\t\tStarting list $list_name\n";
        }
        #Check to see if we learn a superclass
        if($line =~ /^\s*inherit\s*=\s*(.[^,]*),*$/){
          #store the superclass for later reference
          #see above for meaning of $1
          #$1 in this case matches [^,]*, basically anything between inherit= and ,
          print "\tparent $1\n";
          $superclasses{$class_name} = $1;
        }
      }
    } else{
      #Check to see if we enter a class
      if($line =~ /^\s*([^<   #]*)\s*<-.*R6Class/){
        #Extract the name of the class.
        #$1 matches the regex in parentheses [%< \t]* in this case
        $class_name = $1;
        $classes{$class_name} = {};
        $this_class = \%{$classes{$class_name}};
        $documentation{$class_name} = $documentation_buffer;
        $documentation_buffer = '';
        $in_class = 1;
        print "\tclass $class_name\n";
      }
    }
  }
  #Finished with this file
  close(IN);
}


foreach my $class_name (sort keys %classes){
  my $debug = '';
  if($outer_debug =~ "y\n"){
    print "$class_name\n";
    $debug = <>;
  }
  
  
  if(defined($debug) && ($debug eq "y\n")){
    print "$documentation{$class_name}\n";
  }
  print DOC "$class_name\n";
  if(!$documentation{$class_name}){
    print DOC "\t$class_name is undocumented\n";
  } elsif(!($documentation{$class_name} =~ /#' \@export/)){
    print DOC "\t$class_name is undocumented\n";
  } else {
    open(MAN,"> man/$class_name.Rd") or die "Could not open the man file for $class_name at man/$class_name.Rd $!\n";
    print MAN '% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/' . $class_name . '

%Two new commands to help with formatting
\newcommand{\methodtable}{\bold{\cr #1 \cr} \tabular{lcl}}
\newcommand{\methoditem}{\emph{#1} \tab-\tab #2\cr}
\newcommand{\methodname}{\subsection{#1}}
';
    if($documentation{$class_name} =~ /\#' \@docType class/){
      print MAN "\\docType{class}\n";
    } else {
      print DOC "\t$class_name does not specify docType class\n";
    }
    if($documentation{$class_name} =~ /\#' \@description ([^@]*)\n\s*#' \@/s){
      my $desc = $1;
      $desc =~ s/\n\s*#'\s*/ /g;
      print MAN "\\description{$desc}\n";
    } else {
      print DOC "\t$class_name does not have a \@description\n";
    }
    if($documentation{$class_name} =~ /\#' \@title (.*)/){
      print MAN "\\name{$1}\n\\alias{$1}\n\\title{$1}\n";
    } else {
      print DOC "\t$class_name does not have a \@title\n";
    }
    if(($documentation{$class_name} =~ /\#' \@exportClass/)){
      print DOC "\t$class_name is using the S4 export rules\n";
    }
    if(!($documentation{$class_name} =~ /\#' \@importFrom R6 R6Class/)){
      print DOC "\t$class_name does import R6Class\n";
    }
    if(!($documentation{$class_name} =~ /\#' \@keywords .+/)){
      print DOC "\t$class_name does not use keywords\n";
    }
    if(!($documentation{$class_name} =~ /\#' \@example .+/)){
      print DOC "\t$class_name does not have an example.\n";
    }
    if(!($documentation{$class_name} =~ /\#' \@family .+/)){
      print DOC "\t$class_name does not use family to record inheritance\n";
    }
  }
  foreach my $list_name (sort keys %{$classes{$class_name}}){
    if($list_name =~ 'private'){
      next;
    }
    if($list_name =~ 'public'){
      print MAN '\section{Methods}{'."\n";
    }
    if($list_name =~ 'active'){
      print MAN '\section{Fields}{'."\n".'\describe{'."\n";
    }
    foreach my $method_name (sort keys %{$classes{$class_name}->{$list_name}}){
      if($list_name =~ 'public'){
        if(defined($debug) && ($debug eq "y\n")){
          print "DOC: $documentation{$class_name.$list_name.$method_name}\n";
        }
        if(!$documentation{$class_name.$list_name.$method_name}){
          print DOC "\t\t$class_name has undocumented method $method_name.\n";
        } else{
          if($documentation{$class_name.$list_name.$method_name} =~ /#' \@method $method_name (.*)/){
            my $description = $1;
            if($is_abstract{$class_name.$list_name.$method_name}){
              if(!($description =~ /\\bold\{must\} be extended/)){
                print DOC "\t\t\t$class_name does not say that $method_name should be extended\n";
              }
            }
            print MAN "\\methodname{$method_name(".join(',',@{$classes{$class_name}->{$list_name}->{$method_name}}).")}{\n";
            print MAN "$description\n".'\describe{\item{\emph{Arguments}}{\tabular{lcl}{'."\n";
          } else{
            print DOC "\t\t$class_name has not properly declared method $method_name\n";
          }
          foreach my $tmpargument (@{$classes{$class_name}->{$list_name}->{$method_name}}){
            my $argument = "$tmpargument";
            $argument =~ s/\s*=.*//g;
            $argument =~ s/^\.\.\.\s*$/\\\\dots/g;
            #print "$argument\n";
            if(defined($debug) && ($debug eq "y\n")){
              print "ARG: $argument\n";
            }
            if($documentation{$class_name.$list_name.$method_name} =~ /\#' \@param $argument (.*)/){
              my $description = $1;
              if($argument =~ 'dots'){
                $argument =~ s/\\\\dots/\\dots/g;
              }
              print MAN "\\methoditem{$argument}{$description}\n";
            } else {
              print DOC "\t\t\t$argument is undocumented for method $method_name\n";
            }
            #Do some matching to see if its documented...
          }
          print MAN "}}";
          if($documentation{$class_name.$list_name.$method_name} =~ /\#' \@return (.*)/){
            print MAN "\n\\item{Value}{$1}\n";
          } else {
            if(exists($has_return{$class_name.$list_name.$method_name})){
              print DOC "\t\t\t$method_name has no return.\n";
            }
          }
          print MAN "}}\n";
        }
      } else {
        if($list_name =~ 'active'){
          if(scalar(@{$classes{$class_name}->{$list_name}->{$method_name}}) != 1){
            print DOC "\t\tThe active binding $method_name does not have one argument...\n";
          }
          if($documentation{$class_name . $list_name . $method_name} =~ /#' \@field $method_name (.*)/){
            print MAN "\\item{$method_name}{$1}\n";
          } else {
            print DOC "\t\tActive binding $method_name from class $class_name is not properly documented.\n";
          }
        } else{
          die "Something went wrong here: $list_name should be public active or private. \n";
        }
      }
    }
    if($list_name =~ 'public'){
      print MAN "}\n";
    }
    if($list_name =~ 'active'){
      print MAN "}}\n";
    }
  }
  close(MAN);
}

my %depths;
my %final_classes;
#This chunk of code pulls out the number of superclasses a class has and stores it in a hash %depths
#This is used when sorting the classes to create a tree out of them.
#While we're doing this, we also get all of the methods we inherit too
foreach my $class (keys %classes){
  my @tmp = [];
  if(exists($classes{$class}->{'public'})){
    @tmp = keys %{$classes{$class}->{'public'}};
    ##print "$class 's methods should not be inherited:\n\t @tmp \n";
  }
  my $foundRoot = 0;
  my $current = $class;
  $final_classes{$class} = {};
  $depths{$class} = 1;
  #Look for root until we find it
  while($foundRoot == 0){
    foreach my $list (keys %{$classes{$current}}){
      #If the $final_class doesn't have a list, create it
      if(!exists($final_classes{$class}->{$list})){
        $final_classes{$class}->{$list} = {};
      }
      #for each method in the current class, if it exists, then we use the newer version (which we already have)
      #if it doesn't, then we use the version associated with the current class
      foreach my $method (keys %{$classes{$current}->{$list}}){
        if(!exists($final_classes{$class}->{$list}->{$method})){
          if($class !~ $current){
            if(exists($classes{$class}->{$list}->{$method})){
              print "This shouldn't be happening...\n";
            }
            ##print "\t\t\t$class::$list::$method is inherited.\n";
            $is_inherited{$class.$list.$method} = 1;
          }
          #This keeps track of which methods are currently abstract ones.
          if(exists($is_abstract{$current.$list.$method})){
            $is_abstract{$class.$list.$method} = 1;
            $is_abstract{$class} = 1;
            ##print "\t\t\t\t$class::$list::$method is abstract inherited from $current\n";
          }
          $final_classes{$class}->{$list}->{$method} = $classes{$current}->{$list}->{$method};
          $documentation{$class.$list.$method} = $documentation{$current.$list.$method}
        }
      }
    }
    #If we don't have a superclass, then we are the root
    if(!exists($superclasses{$current})){
      $foundRoot = 1;
    } else{
      #If we do, then we recur
      $current = $superclasses{$current};
      ++$depths{$class};
    }
  }
}

foreach my $class_name (sort keys %final_classes){
  my $debug = '';
  if($outer_debug =~ "y\n"){
    $debug = <>;
  }
  
  if(defined($debug) && ($debug eq "y\n")){
  }
  if(!$documentation{$class_name}){
  } elsif(!($documentation{$class_name} =~ /#' \@export/)){
  } else {
    open(MAN,"> man/$class_name.Rd") or die "Could not open the man file for $class_name at man/$class_name.Rd $!\n";
    print MAN '% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/' . $class_name . '

%Two new commands to help with formatting
\newcommand{\methodtable}{\bold{\cr #1 \cr} \tabular{lcl}}
\newcommand{\methoditem}{\emph{#1} \tab-\tab #2\cr}
\newcommand{\methodname}{\subsection{#1}}
';
    if($documentation{$class_name} =~ /\#' \@docType class/){
      print MAN "\\docType{class}\n";
    } else {
    }
    if($documentation{$class_name} =~ /\#' \@title (.*)/){
      print MAN "\\name{$1}\n\\alias{$1}\n\\title{$1}\n";
    } else {
    }
    if($documentation{$class_name} =~ /\#' \@description ([^@]*)\n\s*#' \@/s){
      my $desc = $1;
      $desc =~ s/\n\s*#'\s*/ /g;
      print MAN "\\description{$desc}\n";
    }
    if(($documentation{$class_name} =~ /\#' \@exportClass/)){
    }
    if(!($documentation{$class_name} =~ /\#' \@importFrom R6 R6Class/)){
    }
    if(!($documentation{$class_name} =~ /\#' \@keywords .+/)){
    }
    if($documentation{$class_name} =~ /\#' \@family (.+)/){
      print MAN "\\seealso{\n";
      if($documentation{$superclasses{$class_name}} =~ /#' \@export/){
        print MAN "Inherits from : \\code{\\link{$superclasses{$class_name}}}\n\n";
      }
      my $any_subclasses = 0;
      foreach my $other_class (sort keys %final_classes){
        if(exists($superclasses{$other_class}) and ($superclasses{$other_class} eq $class_name)){
          if($any_subclasses == 0){
            print MAN "Is inherited by : ";
            $any_subclasses = 1;
          } else {
            print MAN ", ";
          }
          print MAN "\\code{\\link{$other_class}}";
        }
      }
      print MAN "}\n";
    }
    if($documentation{$class_name} =~ /\#' \@example (.+)/){
      print "$1\n";
      if(-e "demo/$1"){
        open(EXM,"< demo/$1") or die "Could not open example $1 $!\n";
        print MAN "\\examples{\n";
        while(my $tmp = <EXM>){
          $tmp =~ s/%/\\%/g;
          $tmp =~ s/\s*#.*\n//g;
          $tmp =~ s/{/\\{/g;
          $tmp =~ s/}/\\}/g;
          $tmp =~ s/dontrun\\\{(.*)\\\}/dontrun{$1}/g;
          print MAN $tmp;
        }
        print MAN "}\n";
      } else {
        warn "Could not find example $1\n";
      }
    }
  }
  foreach my $list_name (sort keys %{$final_classes{$class_name}}){
    if($list_name =~ 'private'){
      next;
    }
    if($list_name =~ 'public'){
      print MAN '\section{Methods}{'."\n";
    }
    if($list_name =~ 'active'){
      print MAN '\section{Fields}{'."\n".'\describe{'."\n";
    }
    foreach my $method_name (sort keys %{$final_classes{$class_name}->{$list_name}}){
      if($list_name =~ 'public'){
        if(defined($debug) && ($debug eq "y\n")){
        }
        if(!$documentation{$class_name.$list_name.$method_name}){
        } else{
          if($documentation{$class_name.$list_name.$method_name} =~ /#' \@method $method_name (.*)/){
            print MAN "\\methodname{$method_name(".join(',',@{$final_classes{$class_name}->{$list_name}->{$method_name}}).")}{\n";
            print MAN "$1\n".'\describe{\item{\emph{Arguments}}{\tabular{lcl}{'."\n";
          } else{
          }
          foreach my $tmpargument (@{$final_classes{$class_name}->{$list_name}->{$method_name}}){
            my $argument = "$tmpargument";
            $argument =~ s/\s*=.*//g;
            $argument =~ s/^\.\.\.\s*$/\\\\dots/g;
            if(defined($debug) && ($debug eq "y\n")){
            }
            if($documentation{$class_name.$list_name.$method_name} =~ /\#' \@param $argument (.*)/){
              my $description = $1;
              if($argument =~ 'dots'){
                $argument =~ s/\\\\dots/\\dots/g;
              }
              print MAN "\\methoditem{$argument}{$description}\n";
            } else {
            }
            #Do some matching to see if its documented...
          }
          #HERE
          if(exists($has_return{$class_name.$list_name.$method_name})){
            #print "\@return should be a thing.\n";
          }
          ##Put return statement in if the method has a return
          print MAN "}}";
          if($documentation{$class_name.$list_name.$method_name} =~ /\#' \@return (.*)/){
            print MAN "\n\\item{Value}{$1}\n";
          } else {
            if(exists($has_return{$class_name.$list_name.$method_name})){
              print DOC "\t\t\t$method_name has no return.\n";
            }
          }
          print MAN "}}\n";
        }
      } else {
        if($list_name =~ 'active'){
          if(scalar(@{$final_classes{$class_name}->{$list_name}->{$method_name}}) != 1){
          }
          if($documentation{$class_name . $list_name . $method_name} =~ /#' \@field $method_name (.*)/){
            print MAN "\\item{$method_name}{$1}\n";
          } else {
          }
        } else{
          die "Something went wrong here: $list_name should be public active or private. \n";
        }
      }
    }
    if($list_name =~ 'public'){
      print MAN "}\n";
    }
    if($list_name =~ 'active'){
      print MAN "}}\n";
    }
  }
  close(MAN);
}

# Check to see if all of the classes have test cases for each method about each field.
opendir(TESTS,"tests/testthat/") or die "Could not open testing directory $!\n";
my @testfiles = readdir(TESTS);
my @concrete_tests = grep(/^test-.*.R\s*$/,@testfiles);
my @abstract_tests = grep(/^help-.*.R\s*$/,@testfiles);
open(TEV,"> tests/evaluation.log") or die "Cannot open file test/evaluation.log. $!\n";
print TEV "Evaluating Testing\n";
print "Evaluating Testing\n";
foreach my $class (sort keys %classes){
  my $equivalenceFunction = 0;
  my $testFunction = 0;
  my $testfile;
  my $superclass_tested = 0;
  my %tests;
  #This class is abstract, so it should have a help file.
  my @abstracttestfiles = grep(/^help-$class.R/,@abstract_tests);
  #print "\tABSTRACT - @abstracttestfiles\n";
  my @concretetestfiles = grep(/^test-\d+-$class.R/,@concrete_tests);
  #print "\tCONCRETE - @concretetestfiles\n";
  my @alltestfiles = ();
  push @alltestfiles , @abstracttestfiles;
  push @alltestfiles , @concretetestfiles;
  if((scalar(@abstracttestfiles) + scalar(@concretetestfiles)) == 0){
    print TEV "\t$class has no tests.\n";
  } elsif((scalar(@abstracttestfiles) + scalar(@concretetestfiles)) > 1){
    print TEV "\tclass has too many test files:\n\t\t@abstracttestfiles\n\t\t@concretetestfiles\n"
  } elsif(scalar(@alltestfiles) == 1){
    open(TST,"< tests/testthat/$alltestfiles[0]") or die "Could not open testing file\n";
    print TEV "\tStarted examining tests of $class\n";
    while(<TST>){
      my $line = $_;
      #Check for functions
      if($line =~ /^\s*(.*) *= *function\(.*\).*$/){
        my $function_name = $1;
        if($function_name =~ /is_same_${class}_as/){
          $equivalenceFunction=1;
        }
        if($function_name =~ /test_${class}/){
          $testFunction=1;
        }
      }
      #Check for tests
      if($line =~ /test_that\(\s*paste\(\s*name\s*,\s*"\s*: (.*)".*/){
        if($outer_debug =~ "y\n"){
          print TEV"\t$1\n";
        }
        $tests{$1} = 1;
      }
      #Check for inherited tests
      if(exists($superclasses{$class})){
        if($line =~ /test_$superclasses{$class}\(/){
           $superclass_tested = 1;
         }
      } else {
        $superclass_tested = 1;
      }
    }
    close(TST);
    if($superclass_tested != 1){
      print TEV "\t\t$class does not test its inherited methods.\n";
    }
    if($testFunction!=1){
      print TEV "\t\t$class has no testing function\n";
    }
    if($equivalenceFunction != 1){
      print TEV "\t\t$class has no equivalence relation\n";
    }
    print TEV "\t\tLooking for tests for each method/binding of $class\n";
    foreach my $list_name (sort keys %{$classes{$class}}){
      if($list_name =~ 'private'){
        next;
      }
      foreach my $method_name (sort keys %{$classes{$class}->{$list_name}}){
        if(exists($classes{$superclasses{$class}}->{$list_name}->{$method_name})){
          #print "No need to test $method_name for $class, it will be tested by $superclasses{$class}\n";
          next;
        }
        if($list_name =~ 'public'){
          if(!exists($tests{"$method_name works"})){
            print TEV "\t\t\tNo test for $method_name.\n";
          }
          foreach my $binding_name (sort keys %{$final_classes{$class}->{'active'}}){
            if(!exists($tests{"$method_name respects $binding_name"})){
              print TEV "\t\t\t\tNo test for $method_name and $binding_name\n";
            }
          }
        }
        if($list_name =~ 'active'){
          foreach my $function_name (sort keys %{$final_classes{$class}->{'public'}}){
            if(!exists($tests{"$function_name respects $method_name"})){
              print TEV "\t\t\t\tNo test for $function_name and $method_name\n";
            }
          }
        }
      }
    }
    print TEV "\t\tFinished Looking for tests for each method/binding of $class\n";
    print TEV "\tFinished examining tests of $class\n";
  } else {
    print TEV "Something is wrong...\n";
    warn "Something is wrong...\n";
  }
  #HERE
}
print "Done Evaluating Testing\n";
print TEV "Done Evaluating Testing\n";
close(TEV);

#Generic latex stuff
print OUT '%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{ClassDiagram}
\documentclass{article}
\usepackage[margin=.5in]{geometry}
\usepackage{pbox}
\usepackage{adjustbox}
\usepackage{xcolor}
\usepackage{tikz}
%using linguistics makes the tree smaller apparently
\usepackage[linguistics]{forest}

\usetikzlibrary{positioning}
\usetikzlibrary{automata}
\usetikzlibrary{arrows.meta}

\renewcommand{\emph}[1]{\textcolor{blue}{#1}}
\tikzstyle{umlClass}=[color=black, rectangle split, rectangle split parts=3,text centered, text width = 1.5in,text opacity = 1 ,fill opacity= .3 , draw]
\tikzstyle{abstract}=[fill = green ,fill opacity= .3 ]
\tikzstyle{concrete}=[fill = red,fill opacity= .3 ]
\tikzstyle{inherits}=[-{Latex[length=5mm, width=2mm]},dashed]
\tikzstyle{member}=[-{Latex[length=5mm, width=2mm]},blue]

\begin{document}
\maxsizebox*{.95\textwidth}{.95\textheight}{
  \begin{forest}
    umlClass/.style={color=black, rectangle, text opacity=1,fill opacity=.3,draw}
    abstract/.style={fill = green ,fill opacity= .3 }
    concrete/.style={fill = red,fill opacity= .3 }
';

#sort can't be recursive, so we need a separate subroutine for this
sub my_sort{
  return(recursive_sort($a,$b));
}

#if directly comparable, sort two elements.  If they are not, recur appropriately.
sub recursive_sort{
  my ($a,$b) = @_;
  #print "sorting $a and $b\n";
  #This is the easy one.  If they are the same class, then they are equal
  if($a eq $b){
    #print "\t\tequal\n";
    return(0<=> 0);
  }
  #The root should go first in our ordering
  if(!exists($superclasses{$a})){
    #print "\t\tlhs root\n";
    return(0 <=> 1);
  }
  #The root should go first in our ordering
  if(!exists($superclasses{$b})){
    #print "\t\trhs root\n";
    return(1 <=> 0);
  }
  #if one inherits the other, the superclass should go first
  if($superclasses{$a} eq $b){
    #print "$superclasses{$a} $b\n";
    return(1 <=> 0);
  }
  #if one inherits the other, the superclass should go first
  if($a eq $superclasses{$b}){
    #print "\t\trhs inherits lhs\n";
    return(0 <=> 1);
  }
  #if they have the same superclass, but aren't the same, then alphabetize I guess
  if($superclasses{$a} eq $superclasses{$b}){
    #print "\t\tshared parent\n";
    #print "\t\t";
    #print ( $a cmp $b);
    #print "\n";
    return( $a cmp $b);
  }
  #recur based on how far down the tree we are.  Start by equalizing
  if($depths{$a} > $depths{$b}){
    #print "\t\tlhs deeper than rhs\n\t";
    return(recursive_sort($superclasses{$a},$b));
  }
  if($depths{$b} > $depths{$a}){
    #print "\t\trhs deeper than lhs\n\t";
    return(recursive_sort($a,$superclasses{$b}));
  }
  #print "\t\tequally deep\n\t";
  #at this point they are equally far down, and so we can recur on both to find a common ancestor.
  return(recursive_sort($superclasses{$a},$superclasses{$b}));
  die "We should never have gotten here.\n";
}

sub method_sort{
  ($a <=> $b)
}

#The following code makes the tree.  First we sort the classes by tree order.  See recursive_sort for details of what tree order means
my $previous_depth = 0;
foreach my $class (sort my_sort keys %final_classes){
  #I wanted it to look somewhat pretty.  I gave up on this, but might go back.  The loop over print \t is setting the indent level...
  for(my $counter = 0; $counter < $depths{$class} - $previous_depth; ++$counter){
    for(my $counter2 = ($previous_depth + $counter); $counter2 > 0; --$counter2){
      print OUT "\t";
    }
    #The way latex stores trees is [node [child] [child grandchild]], so we use '[' to mark that
    print OUT "[\n";
  }
  #I wanted it to look somewhat pretty.  I gave up on this, but might go back.  The loop over print \t is setting the indent level...
  for(my $counter = 0; $counter < $previous_depth - $depths{$class}; ++$counter){
    for(my $counter2 = ($previous_depth - $counter-1); $counter2 > 0; --$counter2){
      print OUT "\t";
    }
    #The way latex stores trees is [node [child] [child grandchild]], so we use '[' to mark that
    print OUT "]\n";
  }
  #I wanted it to look somewhat pretty.  I gave up on this, but might go back.  The loop over print \t is setting the indent level...
  for(my $counter = $depths{$class}; $counter > 0; --$counter){
    print OUT "\t";
  }
  #in the case we are not moving through an inheritance path
  if($depths{$class} <= $previous_depth){
    #end the previous path and start a new one from this depth
    print OUT "][";
  }
  #We need this for next time
  $previous_depth = $depths{$class};
  print "$class\n";
  #Latex mumbo-jumbo
  print OUT "\\parbox{\\widthofpbox{\n";
  #print the class name as part of the node
  my $repeated = 0;
  my $previous_line = 0;
  while($repeated < 2){
    print OUT "$class\n";
    #we only want a list of abstract methods, so if a class doesn't have any, we don't want a list
    if((exists($is_abstract{$class})) || ($mode eq 'all')){
      if($repeated > 0){
        print OUT "\\begin{itemize}\n";
      }
    }
    foreach my $list (sort keys %{$final_classes{$class}}){
      foreach my $member (sort keys %{$final_classes{$class}->{$list}}){
        my $output_line = '';
        if(exists($is_abstract{$class.$list.$member})){
          if(exists($is_abstract{$class})){
            if($repeated == 0){
              if($previous_line > 0){
                $output_line = "\\\\";
              }
              $previous_line = 1;
            } elsif($repeated == 1){
              $output_line = "\\item";
            } else{
              die "Something is wrong.  Repeat is $repeated\n";
            }
            $output_line = $output_line . " $list::$member - @{$final_classes{$class}->{$list}->{$member}}\n";
          }
          #$output_line = "$member - @{$final_classes{$class}->{$list}->{$member}}\\\\\n";
          #print "\t\t--$member\n";
          #print "\t\t\t---@{$final_classes{$class}->{$list}->{$member}}\n";
        } else{
          #if($list =~ /active/){
          #if($final_classes{$class}->{$list}->{$member}->[0] =~ /value/){
          #}
          #}
          if(
             ($mode eq 'all') &&
             (!$is_inherited{$class.$list.$member})
            ){
            if($repeated == 0){
              if($previous_line > 0){
                $output_line = "\\\\";
              }
              $previous_line = 1;
            } elsif($repeated == 1){
              $output_line = "\\item";
            } else{
              die "Something is wrong.  Repeat is $repeated\n";
            }
            #$output_line = $output_line . " $member - @{$final_classes{$class}->{$list}->{$member}}\n";
            $output_line = $output_line . " \\emph{ $list::$member - @{$final_classes{$class}->{$list}->{$member}}}";
          }
          #if(!exists($is_abstract{$class})){
          #$output_line = "\\item \\emph{$member} - @{$final_classes{$class}->{$list}->{$member}}";
          #}
          #$output_line = "\\emph{$member} - @{$final_classes{$class}->{$list}->{$member}}\\\\\n";
          #print "\t\t--$member\n";
          #print "\t\t\t---@{$final_classes{$class}->{$list}->{$member}}\n";
        }
        #regex for making things latex-ish
        #can't have bare $
        $output_line =~ s/\$/\\\$/g;
        #can't have bare _
        $output_line =~ s/_/\\_/g;
        #can't have bare ]
        $output_line =~ s/\]/(~/g;
        #can't have bare [
        $output_line =~ s/\[/~)/g;
        #can't have bare {
        #$output_line =~ s/{/(~~/g;
        #can't have bare }
        #$output_line =~ s/}/~~)/g;
        #can't have bare =
        $output_line =~ s/=/{=}/g;
        #this line might be empty (and should be if the method isn't abstract
        print OUT $output_line;
      }
    }
    #we only want a list of abstract methods, so if a class doesn't have any, we don't want a list
    if(exists($is_abstract{$class}) || ($mode eq 'all')){
      if($repeated > 0){
        print OUT "\\end{itemize}\n";
      }
    }
    $repeated = $repeated + 1;
    if($repeated == 1){
      print OUT "}}{\n";
    }
  }
  print OUT "}\n";
  print OUT ',umlClass';
  if(exists($is_abstract{$class})){
    print OUT ',abstract';
  } else{
    print OUT ',concrete';
  }
}
#more formatting stuff and printing off the rest of the tree
for(my $counter = $previous_depth; $counter > 0; --$counter){
  for(my $counter2 = $counter-1; $counter2 > 0; --$counter2){
    print OUT "\t";
  }
  print OUT "]\n";
}
#more latex stuff
foreach my $elem (keys %documentation){
  #print "----$elem----\n";
  #print "===\n";
  #print "$documentation{$elem}";
  #print "===\n\n";
}
print OUT '  \end{forest}
}
\end{document}
';
close(OUT);
close(DOC);
#`pdflatex $outfilename`;

