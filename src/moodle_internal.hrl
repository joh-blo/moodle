-define(APP_NAME,moodle).


-record(trigger,{
	  src::atom(),
	  type::atom(),
	  value::integer(),
	  alarm_type::atom()
	 }).
