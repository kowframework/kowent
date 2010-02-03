

-- This package has some usefull types and their respective KOW_Ent property


--------------
-- Ada 2005 --
--------------
with Ada.Calendar;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent.Properties.Generic_Date;
with KOW_Ent.Properties.Generic_Decimal;
with KOW_Ent.Properties.Generic_Delta;
with KOW_Ent.Properties.Generic_Range;
with KOW_Ent.Properties.Generic_Timestamp;

package KOW_Ent.Extra_Properties is


	----------------------
	-- The Percent Type --
	----------------------

	type Percent is delta 0.01 range -10.00 .. 10.00;
	-- representa porcentagem



	function To_String( P : in Percent ) return String;
	function From_String( Str_P : in String ) return Percent;

	package Percent_Properties is new KOW_Ent.Properties.Generic_Delta(
			Val_Type	=> Percent,
			To_String	=> To_String,
			From_String	=> From_String,
			Null_Value	=> 0.0
		);


	--------------------
	-- The Money Type --
	--------------------

	type Money is delta 0.01 digits 17;
	-- representa moeda, dinheiro.
	

	function To_String( M : in Money ) return String;
	function From_String( Str_M : in String ) return Money;

	package Money_Properties is new KOW_Ent.Properties.Generic_Decimal(
			Val_Type	=> Money,
			To_String	=> To_String,
			From_String	=> From_String,
			Null_Value	=> 0.0
		);

	-------------------
	-- The Date Type --
	-------------------

	subtype Date is Ada.Calendar.Time;

	function To_String( D : in Date ) return String;
	-- format using YYYY-MM-DD
	
	function From_String( Str_D : in String ) return Date;
	-- parse a string in the form YYYY-MM-DD
	-- simply assume it's in the right format and try parsing it... :D
	-- it's a little bit naieve, but works


	package Date_Properties is new KOW_Ent.Properties.Generic_Date(
			Val_Type	=> Date,
			To_String	=> To_String,
			From_String	=> From_String,
			Null_Value	=> Ada.Calendar.Time_Of(
							Year	=> Ada.Calendar.Year_Number'First,
							Month	=> Ada.Calendar.Month_Number'First,
							Day	=> Ada.Calendar.Day_Number'First,
							Seconds	=> 0.0
						)
		);


	------------------------
	-- The Timestamp Type --
	------------------------

	subtype Timestamp is Ada.Calendar.Time;


	No_Timestamp : constant Timestamp := Timestamp(
						Ada.Calendar.Time_Of(
								Year	=> 1948,
								Month	=> 1,
								Day	=> 30,
								Seconds	=> 13.13
							)
				);
	-- date of Mohandas Karamchand Gandhi's death


	function Timestamp_To_String( D : in Timestamp ) return String;
	-- format using YYYY-MM-DD hh:mm:ss
	
	function Timestamp_From_String( Str_D : in String ) return Timestamp;
	-- parse a string in the form YYYY-MM-DD hh:mm:ss
	-- simply assume it's in the right format and try parsing it... :D
	-- it's a little bit naieve, but works


	package Timestamp_Properties is new KOW_Ent.Properties.Generic_Timestamp(
			Val_Type	=> Timestamp,
			To_String	=> Timestamp_To_String,
			From_String	=> Timestamp_From_String,
			Null_Value	=> Ada.Calendar.Time_Of(
							Year	=> Ada.Calendar.Year_Number'First,
							Month	=> Ada.Calendar.Month_Number'First,
							Day	=> Ada.Calendar.Day_Number'First,
							Seconds	=> 0.0
						)
		);


	------------------------
	-- The Dimension Type --
	------------------------

	type Dimension is delta 0.01 range 0.0 .. 10_000_000.0;
	-- we assume the user will always use cm as the dimension unity
	-- TODO :: implement support for multiple dimension unities

	function Dimension_To_String( D : in Dimension ) return String;

	function Dimension_From_String( Str_D : in String ) return Dimension;

	package Dimension_Properties is new KOW_Ent.Properties.Generic_Delta(
			Val_Type	=> Dimension,
			To_String	=> Dimension_To_String,
			From_String	=> Dimension_From_String,
			NUll_Value	=> 0.0
		);


	---------------------
	-- The Weight Type --
	---------------------

	type Weight is delta 0.01 range 0.0 .. 10_000_000.0;
	-- we assume the user will always use g as the weight unity
	-- TODO :: implement support for multiple weight unities

	function Weight_To_String( W : in Weight ) return String;

	function Weight_From_String( Str_W : in String ) return Weight;

	package Weight_Properties is new KOW_Ent.Properties.Generic_Delta(
			Val_Type	=> Weight,
			To_String	=> Weight_To_String,
			From_String	=> Weight_From_String,
			NUll_Value	=> 0.0
		);


	--------------------
	-- The Count Type --
	--------------------

	subtype Count is Natural;

	function Count_To_String( C : in Count ) return String;
	function Count_From_String( Str_C : in String ) return Count;

	package Count_Properties is new KOW_Ent.Properties.Generic_Range(
			Val_Type	=> Count,
			To_String	=> Count_To_String,
			From_String	=> Count_From_String,
			Null_Value	=> 0
		);

	

	-----------------------------------
	-- Operações com Percent e Money --
	-----------------------------------
	
	function "*"( L : in Money; R : in Percent ) return Money;
	-- calcula porcentagem do dinheiro
	
	function "+"( L : in Money; R : in Percent ) return Money;
	-- calcula L + ( R * L )
	

	function "-"( L : in Money; R : in Percent ) return Money;
	-- calcula L - ( R * L )


	--------------------
	-- The Tag Entity --
	--------------------

	-- the Tag entity is to be used to catalog any kind of data in here
	-- it's a generic data backend for your own interface.
	
	type Tag_Entity is new KOW_Ent.Entity_Type with record
		Entity_ID	: KOW_Ent.ID_Type;
		-- the id for the entity being tagged..
	
		Value		: Unbounded_String;
		-- the value is a free form string that represents the content into
		-- tags..
		Entity_Tag	: Unbounded_String;
		-- the entity tag represents the type of the entity being tagged...
		-- it is the 'tag attribute of the entity object.
	end record;
	

	procedure Tag_It( Entity : in KOW_Ent.Entity_Type'Class; Value : in String );
	procedure Tag_It( Entity : in KOW_Ent.Entity_Type'Class; Value : in Unbounded_String );

	-- TODO :: procedures for
	-- 	. querying entities by tag
	-- 	. listing tags for a given etntiy




end KOW_Ent.Extra_Properties;
