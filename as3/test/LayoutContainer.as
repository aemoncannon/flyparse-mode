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

import flash.display.DisplayObject;
import flash.events.ContextMenuEvent;
import flash.events.ErrorEvent;
import flash.events.Event;
import flash.external.ExternalInterface;
import flash.net.URLRequest;
import flash.net.navigateToURL;
import flash.system.Capabilities;
import flash.ui.ContextMenu;
import flash.ui.ContextMenuItem;
import flash.utils.describeType;
import flash.utils.setInterval;
import mx.containers.BoxDirection;
import mx.containers.utilityClasses.BoxLayout;
import mx.containers.utilityClasses.CanvasLayout;
import mx.containers.utilityClasses.Layout;
import mx.effects.EffectManager;
import mx.events.FlexEvent;
import mx.managers.ISystemManager;
import mx.managers.LayoutManager;
import mx.managers.SystemManager;
import mx.resources.ResourceBundle;
import mx.styles.CSSStyleDeclaration;
import mx.styles.IStyleClient;
import mx.styles.StyleManager;

use namespace mx_internal;

//--------------------------------------
//  Styles
//--------------------------------------

include "../styles/metadata/AlignStyles.as";
include "../styles/metadata/GapStyles.as";

/**
 *  Number of pixels between the bottom border
 *  and its content area.  
 *
 *  @default 0
 */
[Style(name="paddingBottom", type="Number", format="Length", inherit="no")]

/**
 *  Number of pixels between the top border
 *  and its content area. 
 *
 *  @default 0
 */
[Style(name="paddingTop", type="Number", format="Length", inherit="no")]

//--------------------------------------
//  Excluded APIs
//--------------------------------------

[Exclude(name="direction", kind="property")]
[Exclude(name="icon", kind="property")]
[Exclude(name="label", kind="property")]
[Exclude(name="tabIndex", kind="property")]
[Exclude(name="toolTip", kind="property")]
[Exclude(name="x", kind="property")]
[Exclude(name="y", kind="property")]

//--------------------------------------
//  Other metadata
//--------------------------------------

/**
 *  Flex defines a default, or Application, container that lets you start
 *  adding content to your module or Application without explicitly defining
 *  another container.
 *  Flex creates this container from the <code>&lt;mx:Application&gt;</code>
 *  tag , the first tag in an MXML application file, or from the
 *  <code>&lt;mx:Module&gt;</code> tag, the first tag in an MXML module file.
 *  While you might find it convenient to use the Application or Module container
 *  as the only  container in your application, in most cases you explicitly
 *  define at least one more container before you add any controls
 *  to your application or module.
 *
 *  @mxml
 *
 *  <p>The <code>&lt;mx:Application&gt;</code> tag inherits all of the tag 
 *  attributes of its superclass and adds the following tag attributes:</p>
 *
 *  <pre>
 *  &lt;mx:Application
 *    <strong>Properties</strong>
 *    layout="vertical|horizontal|absolute"
 *    xmlns:<i>No default</i>="<i>No default</i>"
 * 
 *    <strong>Styles</strong> 
 *    horizontalAlign="center|left|right"
 *    horizontalGap="8"
 *    paddingBottom="0"
 *    paddingTop="0"
 *    verticalAlign="top|bottom|middle"
 *    verticalGap="6"
 *  
 *  /&gt;
 *  </pre>
 *
 *  @see flash.events.EventDispatcher
 */
public class LayoutContainer extends Container
{
	include "../core/Version.as";

	//--------------------------------------------------------------------------
	//
	//  Class initialization
	//
	//--------------------------------------------------------------------------
	
	loadResources();
	
	//--------------------------------------------------------------------------
	//
	//  Class resources
	//
	//--------------------------------------------------------------------------
	[ResourceBundle("core")]

	/**
	*  @private
	*/
	private static var packageResources:ResourceBundle;

	/**
	*  @private
	*/
	private static var viewSourceMenuItem:String;
	/**
	*	@private
	*/
	//--------------------------------------------------------------------------
	//
	//  Class variables
	//
	//--------------------------------------------------------------------------
	
    /**
	 *  @private    
	 */
	mx_internal static var useProgressiveLayout:Boolean = false;

	//--------------------------------------------------------------------------
	//
	//  Class properties
	//
	//--------------------------------------------------------------------------

	//--------------------------------------------------------------------------
	//
	//  Class methods
	//
	//--------------------------------------------------------------------------
	/**
	*  @private
	*  Loads resources for this class.
	*/
	private static function loadResources():void
	{
		viewSourceMenuItem = packageResources.getString("viewSource");	
	}
	
	//--------------------------------------------------------------------------
	//
	//  Constructor
	//
	//--------------------------------------------------------------------------

	/**
	 *  Constructor.
	 */
	public function LayoutContainer()
	{
		super();

		layoutObject.target = this;
		
	}

	//--------------------------------------------------------------------------
	//
	//  Variables
	//
	//--------------------------------------------------------------------------

	/**
	 *  The mx.containers.utilityClasses.Layout subclass that is doing the layout
	 */
	protected var layoutObject:Layout = new BoxLayout();

	/**
	 *  The mx.containers.utilityClasses.Layout subclass that is doing the layout
	 */
	protected var canvasLayoutClass:Class = CanvasLayout;

	/**
	 *  The mx.containers.utilityClasses.Layout subclass that is doing the layout
	 */
	protected var boxLayoutClass:Class = BoxLayout;

	/**
	 *  @private
	 */
	private var resizeHandlerAdded:Boolean = false;

	/**
	 *  @private
	 *  Placeholder for Preloader object reference.
	 */
	private var preloadObj:Object;

	/**
	 *  @private
	 *  Used in progressive layout.
	 */
	private var creationQueue:Array = [];

	/**
	 *  @private
	 *  Used in progressive layout.
	 */
	private var processingCreationQueue:Boolean = false;

	//--------------------------------------------------------------------------
	//
	//  Properties
	//
	//--------------------------------------------------------------------------

	
	//----------------------------------
	//  layout
	//----------------------------------

	/**
	 *  @private
	 *  Storage for layout property.
	 */
	private var _layout:String = ContainerLayout.VERTICAL;

	[Bindable("layoutChanged")]
	[Inspectable(category="General", enumeration="vertical,horizontal,absolute", defaultValue="vertical")]

	/**
	 *  Specifies the layout mechanism used for this application. 
	 *  Applications can use <code>"vertical"</code>, <code>"horizontal"</code>, 
	 *  or <code>"absolute"</code> positioning. 
	 *  Vertical positioning lays out each child component vertically from
	 *  the top of the application to the bottom in the specified order.
	 *  Horizontal positioning lays out each child component horizontally
	 *  from the left of the application to the right in the specified order.
	 *  Absolute positioning does no automatic layout and requires you to
	 *  explicitly define the location of each child component. 
	 *
	 *  @default "vertical"
	 */
	public function get layout():String
	{
		return _layout;
	}

	/**
	 *  @private
	 */
	public function set layout(value:String):void
	{
		if (_layout != value)
		{
			_layout = value;

			if (layoutObject)
				// Set target to null for cleanup.
				layoutObject.target = null;

			if (_layout == ContainerLayout.ABSOLUTE)
				layoutObject = new canvasLayoutClass();
			else
			{
				layoutObject = new boxLayoutClass();

				if (_layout == ContainerLayout.VERTICAL)
				{
					BoxLayout(layoutObject).direction =
						BoxDirection.VERTICAL;
				}
				else
				{
					BoxLayout(layoutObject).direction =
						BoxDirection.HORIZONTAL;
				}
			}

			if (layoutObject)
				layoutObject.target = this;

			invalidateSize();
			invalidateDisplayList();

			dispatchEvent(new Event("layoutChanged"));
		}
	}

	//----------------------------------
	//  usePadding
	//----------------------------------
	
	/**
	 *  @private
	 */
	override mx_internal function get usePadding():Boolean
	{
		// We use padding for all layouts except absolute.
		return layout != ContainerLayout.ABSOLUTE;
	}

	
	//--------------------------------------------------------------------------
	//
	//  Overridden methods: UIComponent
	//
	//--------------------------------------------------------------------------

	
	/**
	 *  @private
	 *  Calculates the preferred, mininum and maximum sizes of the
	 *  Application. See the <code>UIComponent.measure()</code> method for more
	 *  information.
	 *  <p>
	 *  The <code>measure()</code> method first calls
	 *  <code>Box.measure()</code> method, then makes sure the
	 *  <code>measuredWidth</code> and <code>measuredMinWidth</code>
	 *  are wide enough to display the application's control bar.
	 */
	override protected function measure():void
	{
		super.measure();

		layoutObject.measure();
	}

	/**
	 *  @private
	 */
	override protected function updateDisplayList(unscaledWidth:Number,
												  unscaledHeight:Number):void
	{
		super.updateDisplayList(unscaledWidth, unscaledHeight);

		layoutObject.updateDisplayList(unscaledWidth, unscaledHeight);

		// Wait to layout the border after all the children
		// have been positioned.
		createBorder();
	}

	//--------------------------------------------------------------------------
	//
	//  Overridden methods: Container
	//
	//--------------------------------------------------------------------------

	/**
	 *  @private
	 */
	override protected function layoutChrome(unscaledWidth:Number,
											 unscaledHeight:Number):void
	{
		super.layoutChrome(unscaledWidth, unscaledHeight);

		// When Container.autoLayout is false, updateDisplayList()
		// is not called, but layoutChrome() is still called.
		// In that case, we still need to position the border.
		if (!doingLayout)
			createBorder();

	}

	//--------------------------------------------------------------------------
	//
	//  Methods
	//
	//--------------------------------------------------------------------------

}

}
