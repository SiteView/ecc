Exago
======

Introduction
------------

This version of Exago is a remake of the old Exago tool. The idea is to create a version that works, and to bring it forward in terms of functionality. 

What is Exago?
--------------

Exago is a tool which was created to perform data mining on event logs. The vision back in the good old days was to connect a state machine model to a set of events produced by a program, and execute these events in the state machine, thereby giving us insight into how consistent the behavior of a program is. 

In certain types of applications this is great, such as those that generate a limited set of events and whose behaviour closely resembles a simple state machine. Telephony applications may benefit heavily from this approach, since the behaviour of these kinds of systems does closely map to an easily definable state machine. It is not currently known how well this maps to larger programs, since most developers worry about codifying their problem domain rather than writing elegant message (event) logs. :)

Status
------

So far the tool works if you compile it and run the examples, however the reporting facilities are currently quite bare. There is an exago_user module which can be used to test the various bits and pieces of the system as you use it. 

Okay, fine, but how do I run this thing?
----------------------------------------

At the moment you can run it by cloning the repository, then doing:
       make
       ./start-exago.sh

Then once the erlang shell successfully (I hope) starts:
     exago_examples:sms_example().  

You will then be presented with a list of 'instance execution' results, which varies in size but can be quite large in a system with many logs. An instance execution represents the results that a set of events that were grouped, and then ran through a state machine model would produce.

If you wish to see the individual event executions, and how they ran through the state machine sequentially, you should refer to the generated '*.merged_sources' file. The exact filename will be specified when you run the example. This file is very useful and should be used as reference when looking for errors with your state machine model.

Right now you are probably wondering: Why on earth do I have to look through data in order to figure out why my state machine went wrong?  

The answer to this is that Exago in its original form was designed to be run as a batch processing program, and visualization of this data wasn't initially taken into account. We may decide to work on this aspect of Exago in the future, but it is not certain at this point in time.

You can however, obtain a simplified report which will at least give you some statistics about the data, by running:
    R = exago_examples:sms_example().
    exago_printer:print_result(R).

Tutorial 1 - Defining and testing a basic log
=============================================

Introduction
------------

Okay, so you've decided that you would like to use Exago to examine your logs. This tutorial will run through how you would go about using Exago to analyse a simple CSV based log. This log can be interleaved, so messages can come from any different number of sources, but each log message must contain the three required fields "group_id", "timestamp" and "transition_input" - either directly in the message or as a foreign key. Foreign keys will be explained in a later tutorial, so for now you must include the three required fields directly in the log.

Required fields
---------------

The "group_id" is an identifier which represents the instance of the program that the message came from. For example, if you have two separate programs (program 1, and program 2) that write to this log, each message must contain where it came from. The "timestamp" is a timestamp in some format of your choosing. We have some basic timestamp parsers at your disposal if you wish to use them, but if you wish to use a different format, you can do this too (this will be explained later). "transition_input" is the input to the state machine which causes a transition to occur from one state into another.

A simple log, and the one that I will use for this tutorial is:
    1,2010-10-12 16:00:00:0000000,forward
    1,2010-10-12 16:00:01:0000000,forward
    1,2010-10-12 16:00:02:0000000,forward
    1,2010-10-12 16:00:03:0000000,stop

Here, the first field represents the group_id, the second field represents the timestamp, and the third field represents the input to some abstract state machine model.

Testing the log
---------------

The first thing to do is to run the system using ./start-exago.sh. Erlang will then run and you will have access to a shell that contains the functionality required to proceed with the tutorial.

To test that your log file parses adequately as a CSV file, to generate the next representation of the data, or to see what the data format should be before proceeding, run:
   exago_user:test_csv_log(<path-to-log>, <path-to-resulting-data>)

Using the tutorial log, the resulting data looks like this:
      {"1","2010-10-12 16:00:00:0000000","forward"}.
      {"1","2010-10-12 16:00:01:0000000","forward"}.
      {"1","2010-10-12 16:00:02:0000000","forward"}.
      {"1","2010-10-12 16:00:03:0000000","stop"}.

Custom log format
-----------------

The resulting data format shown above is the 'internal data format'. Any other parser for a different format than CSV will work just as well, as long as it generates data in the internal data format, which is a tuple followed by a dot. 

Creating a state machine
------------------------

Next, the system needs to know what state machine you will be using to execute this log. 

The format for a state machine is:
    #state_machine{
	states=[
          #state{number=N, name="State 1"}, ...
        ], 
    	transitions=[
          #transition{from=N, to=M, input="forward"}, ...
        ],
	start=Start,
        accept=[0,1,2...]
    }

There are three things you need to know whilst creating your state machine. First, the state machine must be deterministic for the system to work correctly. Second, you must include the "exago_state_machine.hrl" header to create this record in your code - this is temporary and later will be parsed directly from file. Third, there is a helper function exago_user:test_state_machine(StateMachine, Path) which takes a state machine as its first argument, and a path as its second, that ensures that the state machine is deterministic, and also generates a visualization of the state machine for you. The Path argument is the path to where this visualization should be generated along with the name of the file (e.g input "/path/to/file" -> output "/path/to/file.png"). You can use this visualization to check that your state machine model is correct.

This is the state machine I will be using in this tutorial:
     tutorial_state_machine() ->
        SM = #state_machine{
 	   states=
              [#state{number=0, name="Start"},
	       #state{number=1, name="Room 1"},
	       #state{number=2, name="Room 2"},
	       #state{number=3, name="Room 3"}],
      	   transitions=
	      [#transition{from=0, to=1, input="forward"},
	       #transition{from=1, to=2, input="forward"},
	       #transition{from=2, to=3, input="forward"},
	       #transition{from=3, to=3, input="stop"}],
      	   start=0,
      	   accept=[3]
     	}.

** example state machine visualization here **

Creating a Row Format
---------------------

A row format is an ordered list of field parser functions. The location in the list where the parser functions are placed is important as this is the order that each field row is parsed. You can choose to use the default parsers provided by Exago, or you can replace them with your own. If you do decide to create your own, you should take a look at how the parsers that currently exist in exago_field work. 

This is the row format I'll be using:
     tutorial_row_format() ->
     	[exago_field:parser(group_id),
     	 exago_field:parser(timestamp, "yyyy-MM-dd hh:mm:ss:fffffff"), 
     	 exago_field:parser(transition_input)].

The Field Parsers
-----------------

The list of default parsers are:
    exago_field:parser(timestamp, FormatString)

This represents the location in the row where the timestamp can be found.

    exago_field:parser(annotation, Label)

An annotation is any arbitrary field that you wish to label in some way. The label should be a string. Annotations are useful in connection with input modifiers, as we'll see later.

    exago_field:parser(integer_annotation, Label)

The same as an annotation except the data attached to it is parsed as an integer.

    exago_field:parser(group_id)

The group_id parser expects an integer representing the group_id (as explained above). 

    exago_field:parser(transition_input)

Represents where the transition_input is, can be any kind of data, but parses as a string.

    exago_field:foreign_key(EventSourceName, FieldKey, FieldList, Parser)

This is perhaps the most complicated field parser, and represents a parser for an Id referencing a value found in another log. Even required fields may be specified as foreign keys. For example, if you wish for a group_id to come from a different log named "GroupIdLog", where the field in the "GroupIdLog" is an annotation with the label "ForeignGroupId", you would use: exago_field:foreign_key("GroupIdLog", "ForeignGroupId", [group_id], exago_field:parser(group_id)). The inclusion of this type of parser means that logs can be seen from the perspective of a relational database. This field will be exemplified in a later tutorial.

Ensuring the Field Parsers work correctly
-----------------------------------------

To test your row format against your log data, run the command: 
    exago_user:test_row_format(RowFormat, LogData, ResultPath)

Where the RowFormat is a list of field parsers, the log data is the data file produced by test_csv_log earlier, and the result path is a path of your choice where you wish the resulting data to be placed.

Here is what my result data looks like:
     [{event,{{{2010,10,12},{16,0,3}},0},1,"stop",[],[]},
      {event,{{{2010,10,12},{16,0,2}},0},1,"forward",[],[]},
      {event,{{{2010,10,12},{16,0,1}},0},1,"forward",[],[]},
      {event,{{{2010,10,12},{16,0,0}},0},1,"forward",[],[]}]

We can see that a set of events has been produced and that there are no undefined or erroneous values here.

Running the State Machine
-------------------------

Once you've done all the above steps, you're ready to run your data through the state machine model. 

Heres a summary of what we've done thus far:
      * Tested our log file and generated a data file from it. This data file was used in subsequent tests, and can be used as reference, but is now optional.
      * Created a state machine by using the exago_state_machine.hrl header file, and then tested it by checking that it was deterministic, and by looking at the visualization. 
      * Defined the field format for each row in the log, and thus how it should be parsed. We then used this format to parse the log data, thereby creating some result data representing the parsed source.

So now that we know that our state machine is correct, and that our logs are correctly defined, we can go on to define a function which executes the state machine. 

Here is the function for our tutorial example:
     tutorial_example() ->
     	InputRows    = exago_parser:parse_csv("./src/sample_logs/tutorial.log"),
    	StateMachine = tutorial_state_machine(),
    	RowFormat    = tutorial_row_format(),
    	EventSource  = exago_event:new_source("tutorial", InputRows, RowFormat),
    	Result       = exago_state_machine:analyse_event_source(EventSource, StateMachine),
    	Result.

If you run this function as-is, it will return a list of state machine execution results. To print it into a readable format, use the exago_printer:print_result(Result) function.

For this example it would be:
    exago_printer:print_result(exago_examples:tutorial_example()).
 
The report will give you a short list of information which isn't very detailed, but narrows down where most problems would lie. Here is the report that you get and what you can gauge from it:

* Did the state machine validated correctly - If you ran the state machine test in the beginning of this tutorial, you already know if the state machine is valid, but this is here in case you skip the user friendly testing functionality.
* How many event groups were found in the logs - An event group is the instance of a program or process which generated these events. Every event group is executed by the state machine, the events by default are sorted by time (from earliest to latest) - this is also why timestamps are required fields. 
* How many of event groups finished in an accept state - This tells you whether your program has the behaviour expected by the state machine model or not. 
* How many transitions failed - If your transitions fail it could be a problem with the input modifier or the appropriate transition does not exist when transitioning from one state to the next according to a specific input. Usually this is a problem with your state machine model. This is useful whilst developing the state machine.
* How many states failed - States fail only when they do not exist, so this means that your state machine is missing a state.
* How many constraints fail - Constraints are closures that can test any arbitrary property of your state machine. At the moment the only constraint types supported are time constraints, so if your constraints fail it means that a transition didn't respect some arbitrary time constraint attached to your state machine model. 

Running Exago on Exago
----------------------

Thats all for now! 

** TODO **
