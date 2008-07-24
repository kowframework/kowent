

package Aw_Ent.Values is


	-- Type Declarations --
	
	type Integer_Value is new Value with private;
	-- The main integer type.
	-- Every subtype is an extension of this one - even the bigint - due the 
	--

	subtype Smallint_Value is Integer_Value;
	subtype Bigint_Value is Integer_Value;


	------------------------
	-- CONVERSION METHODS --
	------------------------
	
	-- The following methods should be used to convert from your values
	-- to Aw_Ent's values. These values are then mapped into a
	--
	-- Ordered_Map which is used by Aw_Ent to iterate with the database.

	-- scalar types
	function Get_Value( Value: APQ_Smallint ) return Value_Ptr;
	function Get_Value( Value: APQ_Integer ) return Value_Ptr;
	function Get_Value( Value: APQ_Bigint ) return Value_Ptr;
	function Get_Value( Value: APQ_Real ) return Value_Ptr;
	function Get_Value( Value: APQ_Double ) return Value_Ptr;
	function Get_Value( Value: APQ_Serial ) return Value_Ptr;
	function Get_Value( Value: APQ_Bigserial ) return Value_Ptr;

	-- time types 
	function Get_Value( Value: APQ_Date ) return Value_Ptr;
	function Get_Value( Value: APQ_Time ) return Value_Ptr;
	function Get_Value( Value: APQ_Timestamp ) return Value_Ptr;
	function Get_Value( Value: APQ_Timezone ) return Value_Ptr;


	-- other types
	function Get_Value( Value: APQ_Boolean ) return Value_Ptr;
	function Get_Value( Value: APQ_Bitstring ) return Value_Ptr;
	function Get_Value( Value: String ) return Value_Ptr;
	function Get_Value( Value: Unbounded_String ) return Value_Ptr;


end Aw_Ent.Values;
