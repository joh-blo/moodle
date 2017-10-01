-define(APP_NAME,moodle).


-record(trigger,{
	  src::atom(),
	  type::atom(),
	  value::integer(),
	  alarm_type::atom()
	 }).


-record(alarm,{
	  type::atom(),
	  source::tuple(),
	  max_cnt::integer(),
	  delay::integer()
	 }).

