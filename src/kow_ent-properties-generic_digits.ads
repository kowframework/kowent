


-- Generic package for handling floating point numbers


with APQ;


generic
	type Val_Type is digits <>;
	with function To_String( Element : in Val_Type ) return String;
	with function From_String( Element : in String ) return Val_Type;
	Null_Value : Val_Type;
package KOW_ENT.Properties.Generic_Digits is


	-------------------------------------
	-- Instances for generics from APQ --
	-------------------------------------

	procedure Append is new APQ.Append_Float( Val_Type => Val_Type );
	function Value is new APQ.Float_Value( Val_Type => Val_Type );


	------------------
	-- The Property --
	------------------

	type Getter_Callback is access function( Entity : in Entity_Type'Class ) return Val_Type;
	type Setter_Callback is access procedure( Entity : in out Entity_Type'Class; Value : in Val_Type );

	type Property_Type is new KOW_Ent.Entity_Property_Type with record
		Getter	: Getter_Callback;
		Setter	: Setter_Callback;
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
					Getter		: Getter_Callback;
					Setter		: Setter_Callback;
					Immutable	: in Boolean := False
			) return KOW_Ent.Entity_Property_Ptr;


end KOW_Ent.Properties.Generic_Digits;
