////////////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 2003-2006 Adobe Macromedia Software LLC and its licensors.
//  All Rights Reserved. The following is Source Code and is subject to all
//  restrictions on such code as contained in the End User License Agreement
//  accompanying this product.
//
////////////////////////////////////////////////////////////////////////////////

package mx.core
{
    
    public class UITextField extends FlexTextField implements IAutomationObject, IIMESupport, IFlexModule, IInvalidating, ISimpleStyleClient, IToolTipManagerClient, IUIComponent
    {
	public function test(){
	    var thePeople:XML = <people>
		<person name="Mims Wright" suffix="III">
		<age>27</age>
		<aka>Mims H Wright</aka>
		<aka>Teh AWesoeomes!</aka>
		<bio><![CDATA[This guy <b>rulz<b>!]]></bio>
		</person>
		<person name="Roger Braunstein">
		<age>26</age>
		<aka>Rog</aka>
		
		<aka>That guy</aka>
		<bio><![CDATA[Likes food.]]></bio>
		</person>
		</people>;
	    
	    var thePeople:XML = <people/>;
	    
	    var thePeople:XML = <dude><apple></apple></dude>;
	    
	    var age = thePeople.person.age;
	    
	    var name = thePeople.person.@name;
	    
	    var person = thePeople.person.(age >= 21);
	    
	    var person = thePeople.person[1];
	    
	    var age = thePeople.person.(@name == "Roger Braunstein").age; //XMLList with one node
	    var age = thePeople.person.(@name == "Roger Braunstein")[0].age[0]; //XML
	    
	    var age = 1;
	    
	}
	
    }
    
}
