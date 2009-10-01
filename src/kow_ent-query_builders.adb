------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Entity                             --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2007-2009, Ada Works Project                 --
--                                                                          --
--                                                                          --
-- KOW_Lib is free library;  you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOW_Lib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with KOW_Lib; see file COPYING. If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
------------------------------------------------------------------------------


with Ada.Text_IO;
--------------
-- Ada 2005 --
--------------
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Tags;

---------------
-- Ada Works --
---------------
with KOW_Ent;
with KOW_Ent.Properties;

---------
-- APQ --
---------
with APQ;
with APQ_Provider;

package body KOW_Ent.Query_Builders is
	
	----------------------
	-- Query Management --
	----------------------
	
	procedure Clear( Q : in out Query_Type ) is
		-- clear the query for reuse
	begin
		Operator_Vectors.Clear( Q.Operators );
	end Clear;

	--
	-- Foreign Key
	--

	procedure Append(
				Q		: in out Query_Type;
				Foreign_Key	: in     KOW_Ent.Entity_Type'Class;
				Appender	: in     Logic_Appender := Appender_AND;
				Operator	: in     Logic_Operator := Operator_Equals
			) is
		-- append a query element based on foreign key
		Properties : KOW_Ent.Property_Lists.List;
		Found_Foreign_Key : Boolean := False;

		-- TODO: create an index for foreign key properties in the entity registry

		procedure Iterator( C : in KOW_Ent.Property_Lists.Cursor ) is
			use KOW_Ent.Properties;
			E : KOW_Ent.Entity_Property_Ptr;
		begin
			if Found_Foreign_Key then
				return;
			end if;

			E := KOW_Ent.Property_Lists.Element( C );
			if E.all in Foreign_Key_Property_Type then
				if Foreign_key_Property_Type( E.all ).Related_Entity_Tag = Foreign_Key'Tag then
					Found_Foreign_Key := True;
					Append(
						Q	=> Q,
						Column	=> E.Column_Name,
						Value	=> Foreign_Key.ID,
						Appender=> Appender,
						Operator=> Operator
					);
				end if;
			end if;
		end Iterator;
				
	begin

		-- first we gotta get all the properties for this entity
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity_Type'Tag );

		-- and now we try to find out which one is a foreign key
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
	end Append;


	-- 
	-- KOW_Ent.Id_Type

	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     KOW_Ent.Id_Type;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equals
			) is
	begin
		Append(
			Q		=> Q,
			Column		=> To_Unbounded_String( Column ),
			Value		=> Value,
			Appender	=> Appender,
			Operator	=> Operator
		);
	end Append;

	procedure Append(
				Q	: in out Query_Type;
				Column	: in     Unbounded_String;
				Value	: in     KOW_Ent.Id_Type;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equals
			) is
	begin
		Append(
			Q		=> Q,
			Column		=> Column,
			Value		=> To_Unbounded_String( To_String( Value ) ),
			Appender	=> Appender,
			Operator	=> Operator
		);
	end Append;


	--
	-- String 
	--

	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equals
			) is
		-- append a query element to this query
	begin
		Append(
			Q	=> Q,
			Column	=> To_Unbounded_String( Column ),
			Value	=> To_Unbounded_String( Value ),
			Appender=> Appender,
			Operator=> Operator
		);
	end Append;



	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Unbounded_String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equals
			) is
		-- append a query element to this query
	begin
		Append(
			Q	=> Q,
			Column	=> To_Unbounded_String( Column ),
			Value	=> Value,
			Appender=> Appender,
			Operator=> Operator
		);
	end Append;


	
	procedure Append(
				Q	: in out Query_Type;
				Column	: in     Unbounded_String;
				Value	: in     Unbounded_String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equals
			) is
		-- append a query element to this query
		Handler : Operator_Handler_Type;
	begin
		Handler.Column	:= Column;
		Handler.Value	:= Value;
		Handler.Appender:= Appender;
		Handler.Operator:= Operator;
		Handler.Operation_Type := L_Operator;

		Operator_Vectors.Append( Q.Operators, Handler );
	end Append;




	-- 
	-- Password
	--
	
	procedure Append_Password(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equals
			) is
		-- append a query element to this query
	begin
		Append(
			Q	=> Q,
			Column	=> Column,
			Value	=> KOW_Ent.Calculate_Hash( Value ),
			Appender=> Appender,
			Operator=> Operator
		);

	end Append_Password;



	procedure Append_Password(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Unbounded_String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equals
			) is
		-- append a query element to this query
	begin
		Append_Password(
			Q	=> Q,
			Column	=> Column,
			Value	=> To_String( Value ),
			Appender=> Appender,
			Operator=> Operator
		);
	end Append_Password;

	
	procedure Append_Password(
				Q	: in out Query_Type;
				Column	: in     Unbounded_String;
				Value	: in     Unbounded_String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equals
			) is
		-- append a query element to this query
	begin
		Append_Password(
			Q	=> Q,
			Column	=> To_String( Column ),
			Value	=> To_String( Value ),
			Appender=> Appender,
			Operator=> Operator
		);
	end Append_Password;


	--
	-- Sub Queries
	--




	procedure Append(
				Q	: in out Query_Type;
				Child_Q	: in     Query_Type;
				Appender: in     Logic_Appender := Appender_AND
			) is
		-- append another query as a child query :: () stuff
		Handler : Operator_Handler_Type;
	begin
		Handler.Child_Query := new Query_Type'( Child_Q );
		Handler.Appender := Appender;
		Handler.Operation_Type := Q_Operator;

		Operator_Vectors.Append( Q.Operators, Handler );
	end Append;


	procedure Append_to_APQ_Query(
				Q		: in     Query_Type;
				APQ_Q		: in out APQ.Root_Query_Type'Class;
				Connection	: in out APQ.Root_Connection_Type'Class
			) is
		First_Element : Boolean := True;

		
		procedure Append_Appender( Appender : Logic_Appender ) is
			-- Append the appender.
			-- Use while appending child queries AND operators
		begin
			if First_Element then
				First_Element := False;
				APQ.Append( APQ_Q, " " );
			else
				case Appender is
					when Appender_AND =>
						APQ.Append( APQ_Q, " AND " );
					when Appender_OR  =>
						APQ.Append( APQ_Q, " OR " );
				end case;
			end if;
		end Append_Appender;

		procedure Append_Operator( C : Operator_Vectors.Cursor ) is
			-- an iterator for the operators
			Handler : Operator_Handler_Type := Operator_Vectors.Element( C );
		begin
			Append_Appender( Handler.Appender );
			case Handler.Operation_Type is
				when L_Operator =>
					APQ.Append( APQ_Q, To_String( Handler.Column ) );
					case Handler.Operator is
						when Operator_Equals =>
							APQ.Append( APQ_Q, "=" );
						when Operator_Not_Equals =>
							APQ.Append( APQ_Q, "!=" );
						when Operator_Like =>
							APQ.Append( APQ_Q, " LIKE " );
					end case;
					APQ.Append_Quoted( 
							APQ_Q, 
							Connection, 
							To_String( Handler.Value )
						);

				when Q_Operator =>
					APQ.Append( APQ_Q, "(" );
					Append_to_APQ_Query( Handler.Child_Query.all, APQ_Q, Connection );
					APQ.Append( APQ_Q, ")" );
			end case;
		end Append_Operator;
	begin
		Operator_Vectors.Iterate( Q.Operators, Append_Operator'Access );


		if First_Element then
			-- when it's hasn't processed any element..
			-- a dirty hack for child queries to work fine
			APQ.Append( APQ_Q, "1=1" );
		end if;
	end Append_to_APQ_Query;



	procedure Prepare_and_Run_Query( Q : in Query_Type; Query : in out APQ.Root_Query_Type'Class; Connection : in out APQ.Root_Connection_Type'Class ) is
	

		Info	: Entity_Information_Type := Entity_Registry.Get_Information( Entity_Type'Tag );

	begin
		APQ.Prepare( Query, "SELECT id," );
		Append_Column_Names_For_Read( Query, Info.Properties );
		APQ.Append( Query, " FROM " & To_String( Info.Table_Name ) & " WHERE ");
		Append_to_APQ_Query( Q, Query, Connection );

		-- Ada.Text_IO.Put_line( APQ.To_String( Query ) );


		APQ.Execute( Query, Connection );
	end Prepare_and_Run_Query;

	function Get_All( Q : in Query_Type ) return Entity_Vectors.Vector is
		-- get all results from the query
	
		Results : Entity_Vectors.Vector;
		
		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			Query	: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
		begin
			Prepare_And_Run_Query( Q, Query, Connection );
			begin
				loop
					APQ.Fetch( Query );
					
					declare
						E: Entity_Type;
					begin
						Set_Values_From_Query( E, Query, Connection );
						Entity_Vectors.Append( Results, E );
					end;
		
				end loop;
			exception
				when others => null;
			end;
		end Runner;
	begin
		APQ_Provider.Run( KOW_Ent.My_Provider.all, Runner'Access );
	
		return Results;
	end Get_All;
	

	function Get_First( Q : in Query_Type; Unique : in Boolean ) return Entity_Type is
		-- get the first element from the query
		-- if no results, raise NO_ENTITY
		-- if Unique = True and Tuples( Q ) /= 1 then raise DUPLICATED_ENTITY_ELEMENT.
		Result : Entity_Type;
		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			Query	: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
		begin
			Prepare_And_Run_Query( Q, Query, Connection );
			APQ.Fetch( Query );
			Set_Values_From_Query( Result, Query, Connection );
			begin
				loop
					APQ.Fetch( Query );
					-- if it got here, then check if it's ok to have duplicated results:
					if Unique then
						raise DUPLICATED_ENTITY_ELEMENT with "Tag :: " & Ada.Tags.Expanded_Name( Entity_Type'Tag );
					end if;
				end loop;
			exception
				when APQ.No_Tuple => null;
			end;
		end Runner;
	begin
		APQ_Provider.Run( KOW_Ent.My_Provider.all, Runner'Access );

		return Result;
	exception
		when APQ.No_Tuple =>
			raise NO_ENTITY with "Tag :: " & Ada.Tags.Expanded_Name( Entity_Type'Tag );

	end Get_First;
end KOW_Ent.Query_Builders;
