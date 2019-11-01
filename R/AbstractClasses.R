#---------------------------------------**-------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#--------------Spatial Prediction Model Object Oriented Framework--------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

######################################Generic###################################
#' @importFrom R6 R6Class

fake_instance <- function(object){
  rc <- list()
  class(rc) <- class(object)
  return(rc)
}

#This is a class which contains some helper functionality not part of the R6 package, but that I like.
Generic <- R6Class(
  classname = "Generic",
  private = list(
    #This stores as a string the list of variable names to debug.
    .debug = "",
    #This function checks to see if the type of some input val matches the type
    #of some type$name variable
    #@param name The name of the variable to compare to.  Should be part of the
    #object
    #@param type private or self.
    #@param val The thing to check the type of.  Usually this is not part of the
    #object.
    checkType = function(name,val,type='self'){
      if('checkType' %in% private$.debug){
        browser()
      }
      if(Reduce('&&',
        class(eval(parse(text=paste(type,name,sep='$')))) %in% class(val)
      )){
        return(TRUE)
      }
      #This was to stop distinguishing arrays from matrices.
      #if(
      #  ((class(val) == 'matrix') ||
      #  (class(val) == 'array'))
      #  &&
      #  ((class(eval(parse(text=paste(type,name,sep='$')))) == 'matrix') ||
      #    (class(eval(parse(text=paste(type,name,sep='$')))) == 'array'))
      #){
      #  return(TRUE)
      #}
      return(FALSE)
    },
    defaultActive = function(name,type,value){
      #Method for strongly typed active bindings.
      #@reference Generic::private$checktype
      #@param name Hidden variable associated with the binding.  (Default to .x,
      #where x is the name of the un-hidden version)
      #@param type private or self.  Usually private.
      #@param value The value from the active binding.
      #@example:
        #active = list(
        #  test = function(value){private$defaultActive(name='.test',type=private
        #,value=value)}
        #)
      if('defaultActive' %in% private$.debug){
        browser()
      }
      if(name %in% private$.debug){
        browser()
      }
      #If the value is missing, return the current hidden value
      if(missing(value)){
        return(eval(parse(text=paste(type,name,sep='$'))))
      }
      #If the value is present, check its type before setting the hidden value
      #to it.
      if(!private$checkType(name,value,type)){
        #If the object has all of the types of the current, continue, otherwise
        #throw an error.
        stop(
          paste(
            "invalid data of type",
            paste(
              class(value),collapse=','),
              "expected",
              paste(
                class(
                  #This is equivalent to type$name called from inside the class
                  eval(parse(text=paste(type,name,sep='$')))
                ),
                collapse=','
              )
          )
        )
      }
      #This is equivalent to type$name = value called from inside the class
      eval(parse(text=paste(paste(type,name,sep='$'),'= value')))
    },
    defaultAbstract = function(...){
      #This is a method for abstract methods.
      stop("This is an abstract method")
    }
  ),
  public = list(
    #' @method initialize This function \bold{should} be extended. Create a new instance of this class.
    #' @param \dots This function should take in any arguments just in case.
    initialize = function(...){
      #Uncommenting the line below is sensible, but also causes building to
      #throw a bunch of errors.
      #warning("This is an abstract method")
    },
    #' @method debug A function for debugging the methods of this class.  It calls the \link{browser} command.  In order for methods to opt into to debugging, they need to implement the following code at the beginning: if(<method_name> \%in\% private$.debug)\{browser()\}.  This method exists, because the debugger is not always intuitive when it comes to debugging R6 methods.
    #' @param string The name(s) of methods to debug as a character vector
    debug = function(string){
      #The purpose of this method is to populate the string vector
      #  private$.debug.  Any time a method whose name is in private$.debug is
      #  run, it should call browser()
      #Add it to the debug string, then remove duplicates
      private$.debug = unique(c(string,private$.debug))
    },
    #' @method undebug A function for ceasing to debug methods.  Normally a method will call the \link{browser} command every time it is run.  This command will stop it from doing so.
    #' @param string The name(s) of the methods to stop debugging.
    undebug = function(string){
      #remove it from the debug string
      private$.debug = private$.debug[!(private$.debug %in% string)]
    }
  )
)
