-- Generic package for handling date types



with Ada.Calendar;

with APQ;


generic
	type Val_Type is new Ada.Calendar.Time;
	with function To_String( Element : in Val_Type ) return String;
	with function From_String( Element : in String ) return Val_Type;
	Null_Value : Val_Type;
package KOW_ENT.Properties.Generic_Date is


	-------------------------------------
	-- Instances for generics from APQ --
	-------------------------------------

	procedure Append is new APQ.Append_Date( Val_Type => Val_Type );
	function Value is new APQ.Date_Value( Val_Type => Val_Type );



	------------------
	-- The Property --
	------------------

	type Property_Type is new KOW_Ent.Entity_Property_Type with record
		Getter	: access function( Entity : in KOW_Ent.Entity_Type'Class ) return Val_Type;
		Setter	: access procedure( Entity : in out KOW_Ent.Entity_Type'Class; Value : in Val_type );
	end record;


	overriding
	procedure Set_Property(	
				Property	: in     Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in out APQ.Root_Connection_Type'Class -- the connection that belongs the query
			);
	-- Set the property from the query into the Entity.

	overriding
	procedure Get_Property(
				Property	: in     Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in out APQ.Root_Connection_Type'Class	-- the connection that belongs the query
			);
	-- Append into a query being created by the main KOW_ent engine.


	overriding
	procedure Set_Property(
				Property	: in     Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Value		: in     String				-- the String representation of this value
			);
	-- Set the property from a String representation of the value
	
	overriding
	function Get_Property(
				Property	: in     Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class		-- the entity
			) return String;

	overriding
	procedure Append_Create_Table(
				Property	: in     Property_Type;
				Query		: in out APQ.Root_Query_Type'Class
			);



	function New_Property(
					Column_Name	: in String;
					Getter		: not null access function( Entity : in Entity_Type'Class ) return Val_Type;
					Setter		: not null access procedure( Entity : in out Entity_Type'Class; Value : in Val_Type );
					Immutable	: in Boolean := False
			) return KOW_Ent.Entity_Property_Ptr;


end KOW_Ent.Properties.Generic_Date;
