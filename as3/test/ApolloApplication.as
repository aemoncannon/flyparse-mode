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
import flash.display.Graphics;
import flash.display.Sprite;
import flash.display.NativeWindow;
import flash.display.NativeWindowDisplayState;
import flash.display.NativeWindowResize;
import flash.events.Event;
import flash.events.MouseEvent;
import flash.events.NativeWindowBoundsEvent;
import flash.events.NativeWindowDisplayStateEvent;
import flash.geom.Rectangle;
import flash.system.Capabilities;
import flash.system.Shell;
import flash.text.TextFormat;
import flash.text.TextFormatAlign;
import flash.utils.getQualifiedClassName;
import flash.utils.setTimeout;
import mx.controls.Button;
import mx.core.Application;
import mx.core.EdgeMetrics;
import mx.core.IFlexDisplayObject;
import mx.core.mx_internal;
import mx.core.UITextField;
import mx.core.windowClasses.StatusBar;
import mx.core.windowClasses.TitleBar;
import mx.effects.EffectInstance;
import mx.events.ApolloEvent;
import mx.events.FlexEvent;
import mx.skins.Border;
import mx.skins.halo.ApplicationTitleBarBackgroundSkin;
import mx.skins.halo.StatusBarBackgroundSkin;
import mx.skins.halo.TitleBackground;
import mx.skins.halo.WindowBackground;
import mx.skins.ProgrammaticSkin;
import mx.styles.CSSStyleDeclaration;
import mx.styles.ISimpleStyleClient;
import mx.styles.IStyleClient;
import mx.styles.StyleManager;

use namespace mx_internal;

//--------------------------------------
//  Events
//--------------------------------------

/**
 *  Documentation is not currently available.
 *
 *  @eventType mx.events.ApolloEvent.APPLICATION_ACTIVATE
 */
[Event(name="applicationActivate", type="mx.events.ApolloEvent")]
 
/**
 *  Documentation is not currently available.
 *
 *  @eventType mx.events.ApolloEvent.APPLICATION_DEACTIVATE
 */
[Event(name="applicationDeactivate", type="mx.events.ApolloEvent")]
 
/**
 *  Dispatched before the ApolloApplication window closes.
 * 
 *  @see flash.display.NativeWindow
 */
[Event(name="closing", type="flash.events.Event")]

/**
 *  Dispatched after the display state changes to minimize, maximize
 *  or restore.
 *
 *  @eventType flash.events.NativeWindowDisplayStateEvent.DISPLAY_STATE_CHANGE
 */
[Event(name="displayStateChange", type="flash.events.NativeWindowDisplayStateEvent")]

/**
 *  Dispatched before the display state changes to minimize, maximize
 *  or restore.
 *
 *  @eventType flash.events.NativeWindowDisplayStateEvent.DISPLAY_STATE_CHANGING
 */
[Event(name="displayStateChanging", type="flash.events.NativeWindowDisplayStateEvent")]
 
/**
 *  Dispatched after the ApolloApplication object moves. 
 *
 *  @eventType flash.events.NativeWindowBoundsEvent.MOVE
 */
[Event(name="move", type="flash.events.NativeWindowBoundsEvent")]

/**
 *  Dispatched before the ApolloApplication object moves,
 *  or while the ApolloApplication object is being dragged.
 *
 *  @eventType flash.events.NativeWindowBoundsEvent.MOVING
 */
[Event(name="moving", type="flash.events.NativeWindowBoundsEvent")]
 
/**
 *  Documentation is not currently available.
 *
 *	@eventType flash.events.Event.NETWORK_CHANGE
 */
[Event(name="networkChange", type="flash.events.Event")]
 
 
/**
 *  Temporary event for M3. Dispatched when window's initial 
 *  redraw is done, only when the window is resized in the initialize or creationComplete handler.
 */
[Event(name="initialLayoutComplete", type="flash.events.Event")]

/*
 *  Dispatched after the ApolloApplication object is resized. 
 *
 *  @eventType flash.events.WindowBoundsEvent.RESIZE
 (NOT FOR M3)

[Event(name="resize", type="flash.events.WindowBoundsEvent")] */
 
/*
 *  Dispatched before the ApolloApplication object is resized,
 *  or while the ApolloApplication object boundaries are being dragged.
 *
 *  @eventType flash.events.WindowBoundsEvent.RESIZING
 (NOT FOR M3)

[Event(name="resizing", type="flash.events.WindowBoundsEvent")] */
 
//--------------------------------------
//  Styles
//--------------------------------------

/**
 *  Position of buttons in title bar. Possible values: <code>"left"</code>, 
 *  <code>"right"</code>, <code>"auto"</code>. 
 * 
 *  <p>A value of <code>"left"</code> means the buttons are aligned 
 *  at the left of the title bar.
 *  A value of <code>"right"</code> means the buttons are aligned 
 *  at the right of the title bar.
 *  A value of <code>"auto"</code> means the buttons are aligned
 *  at the left of the title bar on the Macintosh and on the 
 *  right on Windows.</p>
 *
 *  @default "auto"
 */
[Style(name="buttonAlignment", type="String", enumeration="left,right,auto", inherit="yes")]
    
/**
 *  Defines the distance between the titleBar buttons
 *  
 *  @default 2   
 */
[Style(name="buttonPadding", type="Number", inherit="yes")]

/**
 *  Defines the padding added abovce and below the titleBar buttons
 *  
 *  @default 2   
 */
[Style(name="titleBarButtonVerticalPadding", type="Number", inherit="no")]

/**
 *  Style declaration for the skins of the close button on Macintosh.
 *  
 *  @default "applicationButton"
 */
[Style(name="closeButtonMacStyleName", type="String", inherit="yes")]
 
/**
 *  Style declaration for the skins and icon of the close button.
 *  
 *  @default "applicationButton"
 */
[Style(name="closeButtonStyleName", type="String", inherit="yes")]
 
/**
 *  The explicit height of the header. If this style is not set, the header 
 *  height is calculated from the largest of the text height, the button
 *  heights, and the icon height.
 *
 *  @default undefined
 */
[Style(name="headerHeight", type="Number", format="Length", inherit="no")]

/**
 *  Style declaration for skins of the maximize button on Macintosh.
 * 
 *  @default "applicationButton"
 */
[Style(name="maximizeButtonMacStyleName", type="String", inherit="yes")]

/**
 *  Style declaration for skins of the maximize button
 *  on Windows and Linux.
 * 
 *  @default "applicationButton"
 */
[Style(name="maximizeButtonStyleName", type="String", inherit="yes")]

/**
 *  Style declaration for the skins of the minimize button on Macintosh.
 * 
 *  @default "applicationButton"
 */
[Style(name="minimizeButtonMacStyleName", type="String", inherit="yes")]

/**
 *  Style declaration for the skins and icon of the minimize button 
 *  on Windows and Linux.
 * 
 *  @default "applicationButton"
 */
[Style(name="minimizeButtonStyleName", type="String", inherit="yes")]

/**
 *  Style declaration for the skins and icon of the restore button 
 *  on Windows and Linux.
 * 
 *  @default "applicationButton"
 */
[Style(name="restoreButtonStyleName", type="String", inherit="yes")]

/**
 *  Determines whether we display Flex Chrome, or depend on the developer
 *  to draw her own. Changing this style once the window is open has no effect.
 * 
 *  @default true
 */
[Style(name="showFlexChrome", type="Boolean", inherit="no")]

/**
 *  The statusBar background skin.
 *
 *  @default mx.skins.halo.StatusBarBackgroundSkin
 */
[Style(name="statusBackgroundSkin", type="Class", inherit="yes")]

/**
 *  The statusBar class
 *
 *  @default mx.core.windowClasses.StatusBar
 */
[Style(name="statusBarClass", type="Class", inherit="yes")]

/**
 *  A colors used to draw the statusBar.
 *
 *  @default 0xC0C0C0
 */
[Style(name="statusBarBackgroundColor", type="uint", format="Color", inherit="yes")]

/**
 *  Style declaration for alpha of the line between the Status Bar and the 
 *  main part of the application
 * 
 *  @default 0.35
 */
[Style(name="statusBarSeparatorAlpha", type="Number", inherit="yes")]

/**
 *  Style declaration for the color of the line between the Status Bar and the 
 *  main part of the application
 * 
 *  @default 0x000000
 */
[Style(name="statusBarSeparatorColor", type="uint", format="Color", inherit="yes")]

/**
 *  Style declaration for the width of the line between the Status Bar and the 
 *  main part of the application
 * 
 *  @default 1
 */
[Style(name="statusBarSeparatorWidth", type="Number", inherit="yes")]

/**
 *  Style declaration for status text.
 * 
 *  @default undefined
 */
[Style(name="statusTextStyleName", type="String", inherit="yes")]

/**
 *  Position of title in title bar. 
 *  The possible values are <code>"left"</code>, 
 *  <code>"center"</code>, <code>"auto"</code> 
 *  
 *  <p>A value of <code>"left"</code> means the title is aligned
 *  at the left of the title bar.
 *  A value of <code>"center"</code> means the title is aligned
 *  at the center of the title bar.
 *  A value of <code>"auto"</code> means the title is aligned 
 *  at the left on Windows and at the center on the Macintosh.</p>
 * 
 *  @default "auto"
 */
[Style(name="titleAlignment", type="String", enumeration="left,center,auto", inherit="yes")] 

/**
 *  The title background skin.
 *
 *  @default mx.skins.halo.ApplicationTitleBarBackgroundSkin
 */
[Style(name="titleBarBackgroundSkin", type="Class", inherit="yes")]

/**
 *  The distance between the furthest out title bar button and the 
 *  edge of the title bar.
 * 
 *  @default 5
 */
[Style(name="titleBarButtonPadding", type="Number", inherit="true")]

/**
 *  The titleBar class
 *
 *  @default mx.core.windowClasses.TitleBar
 */
[Style(name="titleBarClass", type="Class", inherit="yes")]

/**
 *  An array of two colors used to draw the header.
 *  The first color is the top color.
 *  The second color is the bottom color.
 *  The default values are <code>undefined</code>, which
 *  makes the header background the same as the
 *  panel background.
 *
 *  @default [ 0x000000, 0x000000 ]
 */
[Style(name="titleBarColors", type="Array", arrayType="uint", format="Color", inherit="yes")]

/**
 *  Style declaration for alpha of the inner border of the title bar
 * 
 *  @default 0.2
 */
[Style(name="titleBarInnerBorderAlpha", type="Number", inherit="yes")]

/**
 *  Style declaration for color of the inner border of the title bar
 * 
 *  @default 0xFFFFFF
 */
[Style(name="titleBarInnerBorderColor", type="uint", format="Color", inherit="yes")]

/**
 *  Style declaration for width of the inner border of the title bar
 * 
 *  @default 1
 */
[Style(name="titleBarInnerWidth", type="Number", inherit="yes")]

/**
 *  Style declaration for alpha of the line between the title Bar and the 
 *  main part of the application
 * 
 *  @default 0.35
 */
[Style(name="titleBarSeparatorAlpha", type="Number", inherit="yes")]

/**
 *  Style declaration for the color of the line between the title Bar and the 
 *  main part of the application
 * 
 *  @default 0x000000
 */
[Style(name="titleBarSeparatorColor", type="uint", format="Color", inherit="yes")]

/**
 *  Style declaration for the width of the line between the title Bar and the 
 *  main part of the application
 * 
 *  @default 1
 */
[Style(name="titleBarSeparatorWidth", type="Number", inherit="yes")]

/**
 *  The style name for the title
 *
 *  @default undefined
 */
[Style(name="titleTextStyleName", type="String", inherit="yes")]

//--------------------------------------
//  Effects
//--------------------------------------

/**
 *  Played when the window is closed.
 */
[Effect(name="closeEffect", event="windowClose")]

/**
 *  Played when the component is minimized.
 */
[Effect(name="minimizeEffect", event="windowMinimize")]

/**
 *  Played when the component is unminimized.
 */
[Effect(name="unminimizeEffect", event="windowUnminimize")]

/**
 *  The ApolloApplication defines the application container that you use to 
 *  create Flex applications for Apollo. 
 */
public class ApolloApplication extends mx.core.Application
{
    include "../core/Version.as";
    
    //--------------------------------------------------------------------------
    //
    //  Class assets
    //
    //--------------------------------------------------------------------------    
    [Embed(source="assets/mac_min_up.png")]
    private static var macMinimizeUpSkin:Class;
    

    [Embed(source="assets/win_min_up.png")]
    private static var winMinimizeUpSkin:Class;
    
    [Embed(source="assets/mac_max_up.png")]
    private static var macMaximizeUpSkin:Class;
    
    [Embed(source="assets/win_max_up.png")]
    private static var winMaximizeUpSkin:Class;
    
    [Embed(source="assets/mac_close_up.png")]
    private static var macCloseUpSkin:Class;
    
    [Embed(source="assets/win_close_up.png")]
    private static var winCloseUpSkin:Class;
    
    [Embed(source="assets/win_restore_up.png")]
    private static var winRestoreUpSkin:Class;
    
    [Embed(source="assets/mac_min_over.png")]
    private static var macMinimizeOverSkin:Class;
    
    [Embed(source="assets/win_min_over.png")]
    private static var winMinimizeOverSkin:Class;
    
    [Embed(source="assets/mac_max_over.png")]
    private static var macMaximizeOverSkin:Class;
    
    [Embed(source="assets/win_max_over.png")]
    private static var winMaximizeOverSkin:Class;
    
    [Embed(source="assets/mac_close_over.png")]
    private static var macCloseOverSkin:Class;
    
    [Embed(source="assets/win_close_over.png")]
    private static var winCloseOverSkin:Class;
    
    [Embed(source="assets/win_restore_over.png")]
    private static var winRestoreOverSkin:Class;
    
 	[Embed(source="assets/mac_min_down.png")]
    private static var macMinimizeDownSkin:Class;
    
    [Embed(source="assets/win_min_down.png")]
    private static var winMinimizeDownSkin:Class;
    
    [Embed(source="assets/mac_max_down.png")]
    private static var macMaximizeDownSkin:Class;
    
    [Embed(source="assets/win_max_down.png")]
    private static var winMaximizeDownSkin:Class;
    
    [Embed(source="assets/mac_close_down.png")]
    private static var macCloseDownSkin:Class;
    
    [Embed(source="assets/win_close_down.png")]
    private static var winCloseDownSkin:Class;
    
    [Embed(source="assets/win_restore_down.png")]
    private static var winRestoreDownSkin:Class;
    

    //--------------------------------------------------------------------------
    //
    //  Class constants
    //
    //--------------------------------------------------------------------------

    /**
     *  @private 
     */
    private static const HEADER_PADDING:Number = 4; 
    
    /**
     *  @private 
     */
    private static const MOUSE_SLACK:Number = 5; 
    


  	/**
     *  @private
     */
    private static function initStyles():void
    {
        var winMinButtonSelector:CSSStyleDeclaration = 
        	new CSSStyleDeclaration();
        winMinButtonSelector.defaultFactory = function():void
        {
        	this.upSkin = ApolloApplication.winMinimizeUpSkin;
        	this.overSkin = ApolloApplication.winMinimizeOverSkin;
        	this.downSkin = ApolloApplication.winMinimizeDownSkin;
        }
                
        StyleManager.setStyleDeclaration(".winMinButton", 
        								 winMinButtonSelector, false);
        var winMaxButtonSelector:CSSStyleDeclaration = 
        	new CSSStyleDeclaration();
        winMaxButtonSelector.defaultFactory = function():void
        {
        	this.upSkin = ApolloApplication.winMaximizeUpSkin;
        	this.overSkin = ApolloApplication.winMaximizeOverSkin;
        	this.downSkin = ApolloApplication.winMaximizeDownSkin;
        
        }
        StyleManager.setStyleDeclaration(".winMaxButton", 
        								 winMaxButtonSelector, false);

        var winCloseButtonSelector:CSSStyleDeclaration = 
        	new CSSStyleDeclaration();
        winCloseButtonSelector.defaultFactory = function():void
        {
        	this.upSkin = ApolloApplication.winCloseUpSkin;
        	this.overSkin = ApolloApplication.winCloseOverSkin;
        	this.downSkin = ApolloApplication.winCloseDownSkin;
        }
        StyleManager.setStyleDeclaration(".winCloseButton", 
        								 winCloseButtonSelector, false);								 


        								 
        var macMinButtonSelector:CSSStyleDeclaration = 
        	new CSSStyleDeclaration();
        macMinButtonSelector.defaultFactory = function():void
        {
        	this.upSkin = ApolloApplication.macMinimizeUpSkin;
        	this.overSkin = ApolloApplication.macMinimizeOverSkin;
        	this.downSkin = ApolloApplication.macMinimizeDownSkin;
        	this.alpha=.5;
        }
                
        StyleManager.setStyleDeclaration(".macMinButton", 
        								 macMinButtonSelector, false);
        var macMaxButtonSelector:CSSStyleDeclaration = 
        	new CSSStyleDeclaration();
        macMaxButtonSelector.defaultFactory = function():void
        {
        	this.upSkin = ApolloApplication.macMaximizeUpSkin;
        	this.overSkin = ApolloApplication.macMaximizeOverSkin;
        	this.downSkin = ApolloApplication.macMaximizeDownSkin;
        }
        StyleManager.setStyleDeclaration(".macMaxButton", 
        								 macMaxButtonSelector, false);

        var macCloseButtonSelector:CSSStyleDeclaration = 
        	new CSSStyleDeclaration();
        macCloseButtonSelector.defaultFactory = function():void
        {
        	this.upSkin = ApolloApplication.macCloseUpSkin;
        	this.overSkin = ApolloApplication.macCloseOverSkin;
        	this.downSkin = ApolloApplication.macCloseDownSkin;
        }
        StyleManager.setStyleDeclaration(".macCloseButton", 
        								 macCloseButtonSelector, false);	
        								 
        var winRestoreButtonSelector:CSSStyleDeclaration = 
        	new CSSStyleDeclaration();
        winRestoreButtonSelector.defaultFactory = function():void
        {
        	this.upSkin = ApolloApplication.winRestoreUpSkin;
        	this.overSkin = ApolloApplication.winRestoreOverSkin;
        	this.downSkin = ApolloApplication.winRestoreDownSkin;
        }
        StyleManager.setStyleDeclaration(".winRestoreButton", 
        								 winRestoreButtonSelector, false);	  
        								   						
        var titleTextSelector:CSSStyleDeclaration = 
        	new CSSStyleDeclaration();
        titleTextSelector.defaultFactory = function():void
        {
        	this.color = 0x585858;
        	this.fontSize = 10;
        	this.fontFamily = "Verdana";
        	this.alpha = 0.6;
        }



		StyleManager.setStyleDeclaration(".titleTextStyle",
										 titleTextSelector, false);
							 
		var statusTextSelector:CSSStyleDeclaration =
			new CSSStyleDeclaration();
		
        
		statusTextSelector.defaultFactory = function():void
        {
        	this.color = 0x585858;
        	this.fontSize = 9;
        	this.alpha = 0.6;
        }			
		StyleManager.setStyleDeclaration(".statusTextStyle",
										 statusTextSelector, false);	  		
  		
  		var selector:CSSStyleDeclaration =
        		    StyleManager.getStyleDeclaration("ApolloApplication");
        if (!selector)
            selector = new CSSStyleDeclaration();




        selector.defaultFactory = function():void
        {
            this.borderColor = 0xA6A6A6;
            this.borderStyle = "solid";
            this.borderThickness = 1;
            this.buttonPadding = 2;
            this.titleBarButtonVerticalPadding = 6;
            this.closeButtonMacStyleName = "macCloseButton";
            this.closeButtonStyleName = "winCloseButton";
            this.cornerRadius = 8;
            this.backgroundColor = 0xc0c0c0;
            this.backgroundImage = WindowBackground;
            this.buttonAlignment = "auto";
            this.showFlexChrome = true;
            this.highlightAlphas = [ 1.0, 1.0 ];
            this.maximizeButtonMacStyleName = "macMaxButton";
            this.maximizeButtonStyleName = "winMaxButton";
            this.minimizeButtonMacStyleName = "macMinButton";
            this.minimizeButtonStyleName = "winMinButton";            
            this.restoreButtonStyleName = "winRestoreButton";
            this.roundedBottomCorners = false;
            this.statusBackgroundSkin = StatusBarBackgroundSkin;
            this.statusBarBackgroundColor = 0xCCCCCC;
            this.statusBarClass = StatusBar;
            this.statusBarSeparatorWidth = 1;
            this.statusBarSeparatorColor = 0x000000;
            this.statusBarSeparatorAlpha = 0.35;
            this.statusTextStyleName = "statusTextStyle";
            this.titleAlignment = "auto";
            this.titleBarButtonPadding = 5;
            this.titleBarInnerBorderAlpha = 0.2;
            this.titleBarInnerBorderColor = 0xFFFFFF;
            this.titleBarInnerBorderWidth = 1;                    
            this.titleBarSeparatorAlpha = 0.35;
            this.titleBarSeparatorColor = 0x000000;
            this.titleBarSeparatorWidth = 1;
            this.titleBarBackgroundSkin = ApplicationTitleBarBackgroundSkin;
            this.titleBarClass = TitleBar;
            this.titleBarColors = [ 0xFFFFFF, 0xBABABA ];
            this.titleTextStyleName = "titleTextStyle";
        }
        
        StyleManager.setStyleDeclaration("ApolloApplication", selector, false);
    }

    initStyles();   

    //--------------------------------------------------------------------------
    //
    //  Constructor
    //
    //--------------------------------------------------------------------------

    /**
     *  Constructor.
     */
    public function ApolloApplication()
    {
        super();
        
     	addEventListener(MouseEvent.MOUSE_DOWN, mouseDownHandler);
        addEventListener(FlexEvent.CREATION_COMPLETE, creationCompleteHandler);
        addEventListener(FlexEvent.UPDATE_COMPLETE, updateComplete_handler);
        
        var shell:Shell = Shell.shell;
 		shell.addEventListener(Event.ACTIVATE, shell_activateHandler);
 		shell.addEventListener(Event.DEACTIVATE, shell_deactivateHandler);
 		shell.addEventListener(Event.NETWORK_CHANGE,
 							   shell_networkChangeHandler);
 				
    }
    
    //--------------------------------------------------------------------------
    //
    //  Variables
    //
    //--------------------------------------------------------------------------
 
 	/**
     *  @private
     */
 	private var _maximized:Boolean = false;
 
    /**
     *  @private
     */
    private var appViewMetrics:EdgeMetrics;
    
    /**
     *  @private
     *  A reference to this Application's title bar skin.
     *  This is a child of the titleBar.
     */
    mx_internal var titleBarBackground:IFlexDisplayObject;
    
    /**
     *  @private
     *  A reference to this Application's status bar skin.
     *  This is a child of the statusBar.
     */
    mx_internal var statusBarBackground:IFlexDisplayObject;
    
    /**
    *  @private
    */
    private var oldX:Number;
    
    /**
    *  @private
    */
    private var oldY:Number;
    
    /**
     *  @private 
     */
    private var prevX:Number;
    
    /**
     *  @private 
     */
    private var prevY:Number;
      
    //--------------------------------------------------------------------------
    //
    //  Overridden properties: Container
    //
    //--------------------------------------------------------------------------

    //----------------------------------
    //  viewMetrics
    //----------------------------------

    /**
     *  @private  
     *
     *  Returns the thickness of the edges of the object, including  
     *  the border, title bar, and scroll bars, if visible.
     *  @return Object with left, right, top, and bottom properties
     *  containing the edge thickness, in pixels.
     */
    override public function get viewMetrics():EdgeMetrics

    {

  		var bm:EdgeMetrics = super.viewMetrics;
        var vm:EdgeMetrics = new EdgeMetrics(bm.left, bm.top,
        									 bm.right, bm.bottom);

		
   
        // Since the header covers the solid portion of the border,  
        // we need to use the larger of borderThickness or headerHeight
        var hHeight:Number = getHeaderHeight();
        if (!isNaN(hHeight))
            vm.top += hHeight;
        
        if (_showStatusBar)
        {
            var sHeight:Number = getStatusBarHeight();
            if (!isNaN(sHeight))
                vm.bottom += sHeight;
        }
        if (controlBar && controlBar.includeInLayout)
        {
            vm.top += controlBar.getExplicitOrMeasuredHeight();
        }

        return vm;
    }
    
    //--------------------------------------------------------------------------
    //
    //  Properties
    //
    //--------------------------------------------------------------------------
    
    //----------------------------------
    //  bounds
    //----------------------------------

    /**
     *  @private 
     *  Storage for the bounds property.
     */
    private var _bounds:Rectangle = new Rectangle(0,0,0,0);
    
    /**
     *  @private 
     */
    private var boundsChanged:Boolean = false;

    /**
     *  A Rectangle specifying the Application's bounds, relative to the screen.
     */
    public function get bounds():Rectangle
    {
        return _bounds;
    }
    
    /**
     *  @private
     */ 
    public function set bounds(value:Rectangle):void
    {
        _bounds = value;
        boundsChanged = true;
        
        invalidateProperties();
        invalidateSize();
        invalidateViewMetricsAndPadding();
    }
    
   	//----------------------------------
    //  height
    //----------------------------------
	/**
     *  Set the height. Also set the stage height. 
     * 
     */
     override public function set height(value:Number):void
     {
     	if (value < minimumHeight)
     		value = minimumHeight;
     	else if (value > maximumHeight)
     		value = maximumHeight;
     	_bounds.height = value;
     	boundsChanged = true;
        
        invalidateProperties();
        invalidateSize();
        invalidateViewMetricsAndPadding();
     }
	
    //----------------------------------
    //  resizable
    //----------------------------------

    /**
     *  @private 
     *  Storage for the resizable property.
     */ 
    private var _resizable:Boolean = true;

    /**
     *  A flag that determines whether the window is resizable.
     * 
     *  @default true
     */
    public function get resizable():Boolean
    {
        return _resizable;
    }

    /**
     *  @private
     */
    public function set resizable(value:Boolean):void
    {
        _resizable = value;
    }
    
    //----------------------------------
    //  showStatusBar
    //----------------------------------

    /**
     *  A flag that determines whether the status bar is shown.
     * 
     *  @default false
     */
    private var _showStatusBar:Boolean = true;
    
    /**
     *  @private 
     */
    private var showStatusBarChanged:Boolean = true;

    /**
     *  If <code>true</code>, the status bar is visible.
     *
     *  @default true
     */
    public function get showStatusBar():Boolean
    {
        return _showStatusBar;
    }
    
    /**
     *  @private
     */ 
    public function set showStatusBar(value:Boolean):void
    {
        if (_showStatusBar == value)
            return;
        
        _showStatusBar = value;
        showStatusBarChanged = true;
        
        invalidateProperties();
        invalidateDisplayList();
    }
   	//----------------------------------
    //  width
    //----------------------------------
	/**
     *  Set the width. Also set the stage width. 
     * 
     */
     override public function set width(value:Number):void
     {
     	if (value < minimumWidth)
     		value = minimumWidth;
     	else if (value > maximumWidth)
     		value = maximumWidth;
     	
     	_bounds.width = value;
     	boundsChanged = true;
        
        invalidateProperties();
        invalidateSize();
        invalidateViewMetricsAndPadding();
     }
	
	//----------------------------------
	// maximum height
	//----------------------------------
	
	/**
	 *  Specifies the maximum height of the application's window
	 * 
	 *  @default undefined
	 */
	 public var maximumHeight:Number;
	 
	//----------------------------------
	// maximum width
	//----------------------------------
	
	/**
	 *  Specifies the maximum width of the application's window
	 * 
	 *  @default undefined
	 */
	 public var maximumWidth:Number;
	 
	 //---------------------------------
	 // minimum height
	 //---------------------------------
	 
	 /**
	 *  Specifies the minimum height of the application's window
	 * 
	 *  @default 100;
	 */
	 public var minimumHeight:Number = 100;
	 
	 //---------------------------------
	 // minimum width
	 //---------------------------------
	 
	 /**
	 *  Specifies the minimum width of the application's window
	 * 
	 *  @default 100;
	 */
	 public var minimumWidth:Number = 100;
	 
    //----------------------------------
    //  status
    //----------------------------------

    /**
     *  @private
     */
    private var _status:String = "";
    
    /**
     *  @private
     */
    private var statusChanged:Boolean = false;
    
    /**
     *  The string that appears in the status bar, if it is visible.
     * 
     *  @default ""
     */
    [Bindable("statusChanged")]
    public function get status():String
    {
        return _status;
    }    

    /**
     *  @private
     */
    public function set status(value:String):void
    {
       
       	_status = value;
       	statusChanged = true;
        invalidateProperties();
        invalidateSize();
        invalidateViewMetricsAndPadding();
        dispatchEvent(new Event("statusChanged"));
    }
        
    //----------------------------------
    //  statusBar
    //----------------------------------

    /**
     *  The UIComponent that displays the status bar. 
     */ 
    protected var statusBar:Object;
    
    //----------------------------------
    //  title
    //----------------------------------

    /**
     *  Storage for the title.
     */
    private var _title:String = "";
        
    /**
     *  @private
     */
    private var titleChanged:Boolean = false;

    /**
     *  The title that appears in the window title bar and 
     *  the dock/taskbar.
     * 
     *  @default ""
     */
    [Bindable("titleChanged")]
    public function get title():String
    {
        return _title;
    }
    /**
     *  @private
     */
    public function set title(value:String):void
    {
       	titleChanged = true;
       	_title = value;
        
        invalidateProperties();
        invalidateSize();
        invalidateViewMetricsAndPadding();
        invalidateDisplayList();
        dispatchEvent(new Event("titleChanged"));
    }
    
    //----------------------------------
    //  titleBar
    //----------------------------------

    /**
     *  A reference to the title bar.
     */
    protected var titleBar:Object;

	//---------------------------------
	//   titleIcon
	//---------------------------------
	
    /**
     *  @private
     *  A reference to this container's title icon.
     */
    private var _titleIcon:Class;
    
    /**
     *  The Class (usually an image) used to draw the title bar icon.
     * 
     *  @default null
     */
    [Bindable("titleIconChanged")]
    public function get titleIcon():Class
    {
        return _titleIcon;
    }
        
    /**
     *  @private
     */
    public function set titleIcon(value:Class):void
    {
       	_titleIcon = value;
        
        invalidateProperties();
        invalidateSize();
        invalidateViewMetricsAndPadding();
        invalidateDisplayList();
        dispatchEvent(new Event("titleIconChanged"));
    }
    
    
    /**
     *   The Window that the app's in. 
     *   read-only
     */
    public function get window():NativeWindow
    {
    	return systemManager.stage.window;
    }

    //--------------------------------------------------------------------------
    //
    //  Overridden methods: UIComponent
    //
    //--------------------------------------------------------------------------

    /**
     *  @private
     */
    override protected function createChildren():void
    {
        super.createChildren();
        if (getStyle("showFlexChrome") == false)
        {
        	setStyle("borderStyle", "none");
        	setStyle("backgroundAlpha", 0);
        	return;
        }
        if (!statusBar)
        {
          	var statusBarClass:Class = getStyle("statusBarClass");
          	if (statusBarClass)
          	{
          		statusBar = new statusBarClass();
          		var statusBarDataRenderer:IDataRenderer = 
            		statusBar as IDataRenderer;
            	if (statusBarDataRenderer)
            		statusBarDataRenderer.data = this;
            	rawChildren.addChild(DisplayObject(statusBar));
            	statusBar.styleName = this;
      	        showStatusBarChanged = true;
          	}
        }
        if (systemManager.stage.window.initOptions.systemChrome != "none")
        {
        	setStyle("borderStyle", "none");
            return;   
        }
                  
        if (!titleBar)
        {
            var titleBarClass:Class = getStyle("titleBarClass");
            if ( titleBarClass )
            {
            	titleBar = new titleBarClass();
            	var titleBarDataRenderer:IDataRenderer = 
            		titleBar as IDataRenderer;
            	if (titleBarDataRenderer)
            		titleBarDataRenderer.data = this;
            	rawChildren.addChild(DisplayObject(titleBar));
            	titleBar.styleName = this;
            	
            }
            titleBar.doubleClickEnabled = true;
        	titleBar.addEventListener(MouseEvent.DOUBLE_CLICK, titleBar_doubleClickHandler);
        }
    }
    
    /**
     *  @private
     */
    override protected function commitProperties():void
    {
        super.commitProperties();
        
        if (boundsChanged)
        {
            systemManager.stage.stageWidth = _bounds.width;
            systemManager.stage.stageHeight = _bounds.height;
            boundsChanged = false;
        }
        
        if (titleChanged)
        {
            systemManager.stage.window.title = _title;
            titleChanged = false;
        }
        
        if (showStatusBarChanged)
        {
            if (statusBar)
	            statusBar.visible = _showStatusBar;
            showStatusBarChanged = false;
        }
        
        if (statusChanged)
        {
        	statusChanged = false;
        }
     }
     
    /**
     *  @private
     */
    override protected function measure():void
    {
        if (_maximized)
        {
        	_maximized = false;
        	systemManager.stage.window.maximize();
        }
        
        super.measure();

		//might need some stuff in here for dropShadow, etc.
    }
    
    /**
     *  @private
     */
    override protected function updateDisplayList(unscaledWidth:Number, 
                                                  unscaledHeight:Number):void
    {  
        super.updateDisplayList(unscaledWidth, unscaledHeight);
        //Application.updateDisplayList can change the width and height. 
        //We use their new values

       	unscaledWidth = width;
        unscaledHeight = height;

        var bm:EdgeMetrics = borderMetrics;
        
        var leftOffset:Number = 10;
        var rightOffset:Number = 10;
        
        if (statusBar)
        {
        	statusBar.move(bm.left, unscaledHeight - bm.bottom - getStatusBarHeight());
        	statusBar.setActualSize(unscaledWidth - bm.left - bm.right, getStatusBarHeight());
        
        }
               
        if (systemManager.stage.window.initOptions.systemChrome != "none")
            return;

        var buttonAlign:String = 
			String(getStyle("buttonAlignment"));
        if (titleBar)
        {
	        titleBar.move(bm.left, bm.top);
	        titleBar.setActualSize(unscaledWidth - bm.left - bm.right, 
	                               getHeaderHeight());
        }                
        if (titleBar && controlBar)
        	controlBar.move(0, titleBar.height);
    }

    //--------------------------------------------------------------------------
    //
    //  Methods
    //
    //--------------------------------------------------------------------------

    /**
    *  Closes the app. Will dispatch cancelable event.
    */
    public function close():void
    {
    	var e:Event = new Event("closing", false, true);
    	stage.window.dispatchEvent(e);
    }
   
    /**
     *  @private
     *  Returns the height of the header.
     */
    private function getHeaderHeight():Number
    {
        if (getStyle("headerHeight") != null)
        	return getStyle("headerHeight");
        if (systemManager.stage.window.initOptions.systemChrome != "none")
            return 0;
        if (titleBar)
	        return(titleBar.getExplicitOrMeasuredHeight());
	    return 0;
    }    
    
    /**
     *  @private
     *  Returns the height of the statusBar.
     */
    public  function getStatusBarHeight():Number
    {
     /*   var statusHeight:Number = Math.max(
        	measureChromeText(statusTextField).height +
        	UITextField.TEXT_HEIGHT_PADDING, 17);*/
        if (statusBar)
	        return statusBar.getExplicitOrMeasuredHeight();
	    return 0;
    }

    /**
     *  @private
     */     
    private function measureChromeText(textField:UITextField):Rectangle
    {
        var textWidth:Number = 20;
        var textHeight:Number = 14;
        
        if (textField && textField.text)
        {
            textField.validateNow();
            textWidth = textField.textWidth;
            textHeight = textField.textHeight;
        }
        
        return new Rectangle(0, 0, textWidth, textHeight);
    }
        
    /**
     *  @private
     *  Starts a system move.
     */ 
    private function startMove(event:MouseEvent):void
    {
        addEventListener(MouseEvent.MOUSE_MOVE, mouseMoveHandler);
        addEventListener(MouseEvent.MOUSE_UP, mouseUpHandler);
        
        prevX = event.stageX;
        prevY = event.stageY;
    }
    
    /**
     *  @private
     *  Starts a system resize.
     */ 
    private function startResize(start:String):void
    {
        if (resizable)
            stage.window.startResize(start);
    }

    //--------------------------------------------------------------------------
    //
    //  Event handlers
    //
    //--------------------------------------------------------------------------


    /**
     *  @private
     */
    public function minimize():void
    {
        var e:NativeWindowDisplayStateEvent = new NativeWindowDisplayStateEvent(
        		NativeWindowDisplayStateEvent.DISPLAY_STATE_CHANGING, 
        		false, true, window.displayState,
        		NativeWindowDisplayState.MINIMIZED)
        stage.window.dispatchEvent(e);
        if (!e.isDefaultPrevented())
        	stage.window.minimize();
        
    }

    /**
     *  @private
     */
    private function windowMinimizeHandler(event:Event):void
    {
        stage.window.minimize();
        removeEventListener("effectEnd", windowMinimizeHandler);
    }
    
    /**
    *  @private
    */
    private function windowUnminimizeHandler(event:Event):void
    {   
        removeEventListener("effectEnd", windowUnminimizeHandler);
    }

    /**
     *  @private
     */
    public function maximize():void
    {
        if (stage.window.displayState == NativeWindowDisplayState.MAXIMIZED)
        {
            var e:NativeWindowDisplayStateEvent = new NativeWindowDisplayStateEvent(
                        NativeWindowDisplayStateEvent.DISPLAY_STATE_CHANGING,
                        false, true, NativeWindowDisplayState.MAXIMIZED, 
                        NativeWindowDisplayState.NORMAL);
            stage.window.dispatchEvent(e);
            if (!e.isDefaultPrevented())
            	window.restore();
        }
        else
        {
         	var f:NativeWindowDisplayStateEvent = new NativeWindowDisplayStateEvent(
                        NativeWindowDisplayStateEvent.DISPLAY_STATE_CHANGING,
                        false, true, stage.window.displayState, 
                        NativeWindowDisplayState.MAXIMIZED);
            stage.window.dispatchEvent(f);
        	if (!f.isDefaultPrevented())
        	   window.maximize();    
        }
    }
    
    

    /**
     *  @private
     */
    private function window_moveHandler(event:NativeWindowBoundsEvent):void
    {
        dispatchEvent(new Event("movefx", false));
        dispatchEvent(event);
    }
    
    /**
     *  @private
     */ 
    private function window_displayStateChangeHandler(
                    		event:NativeWindowDisplayStateEvent):void
    {
        //redispatch event 
        dispatchEvent(event);
    }
    
    /**
     *  @private
     */ 
    private function window_displayStateChangingHandler(
                    		event:NativeWindowDisplayStateEvent):void
    {
        //redispatch event for cancellation purposes
        dispatchEvent(event);
        if (event.isDefaultPrevented())
        	return; 
        if (event.afterDisplayState == NativeWindowDisplayState.MINIMIZED)
        {
            if (getStyle("minimizeEffect")) 
            {
                event.preventDefault();
                addEventListener("effectEnd", windowMinimizeHandler);
                dispatchEvent(new Event("windowMinimize")); 
            } 
        } 
              
        // After here, afterState is normal
        else if (event.beforeDisplayState == NativeWindowDisplayState.MINIMIZED)
        {
            addEventListener("effectEnd", windowUnminimizeHandler);
            dispatchEvent(new Event("windowUnminimize"));
        } 
    }
    
    /**
     *  @private
     */ 
    private function windowMaximizeHandler(event:Event):void
    {
        removeEventListener("effectEnd", windowMaximizeHandler);
        stage.window.maximize();
    }
    
	/**
     *  @private
     */ 
    private function windowUnmaximizeHandler(event:Event):void
    {
        removeEventListener("effectEnd", windowUnmaximizeHandler);
        stage.window.restore();
    }
    
    /**
     *  @private
     */
    private function mouseDownHandler(event:MouseEvent):void
    {
        if (systemManager.stage.window.initOptions.systemChrome != "none")
            return;
        
        var dragWidth:int = Number(getStyle("borderThickness")) + 6;
        var cornerSize:int = 12;
        // we short the top a little
        
        if (event.stageY < Number(getStyle("borderThickness")))
        {
            if (event.stageX < cornerSize)
                startResize(NativeWindowResize.TOP_LEFT);
            else if (event.stageX > width - cornerSize)
                startResize(NativeWindowResize.TOP_RIGHT);
            else
                startResize(NativeWindowResize.TOP);
        }
        
        // If it's below the border, but over the titlebar, 
        // we move instead of resize.
        else if (event.stageY < getHeaderHeight() +  
                 Number(getStyle("borderThickness")))
        {
            stage.window.startMove();
        }
        
        else if (event.stageY > (height - dragWidth))
        {
            if (event.stageX < cornerSize)
                 startResize(NativeWindowResize.BOTTOM_LEFT);
            else if (event.stageX > width - cornerSize)
                startResize(NativeWindowResize.BOTTOM_RIGHT);
            else
                startResize(NativeWindowResize.BOTTOM);
        }
        
        else if (event.stageX < dragWidth )
        {
            if (event.stageY < cornerSize)
                startResize(NativeWindowResize.TOP_LEFT);
            else if (event.stageY > height - cornerSize)
                startResize(NativeWindowResize.BOTTOM_LEFT);
            else
                startResize(NativeWindowResize.LEFT);
        }
        
        else if (event.stageX > width - dragWidth)
        {
            if (event.stageY < cornerSize)
                startResize(NativeWindowResize.TOP_RIGHT);
            else if (event.stageY > height - cornerSize)
                startResize(NativeWindowResize.BOTTOM_RIGHT);
            else
                startResize(NativeWindowResize.RIGHT);
        }
    }

    /**
     *  @private
     */ 
    private function closeButton_clickHandler(event:Event):void
    {
       stage.window.close();
    }
        
    /**
     *  @private
     */ 
    private function creationCompleteHandler(event:Event = null):void
    {
        systemManager.stage.window.addEventListener(
            NativeWindowDisplayStateEvent.DISPLAY_STATE_CHANGING, 
            window_displayStateChangingHandler);
        systemManager.stage.window.addEventListener(
            NativeWindowDisplayStateEvent.DISPLAY_STATE_CHANGE, 
            window_displayStateChangeHandler)

        systemManager.stage.window.addEventListener(
            "closing", window_closingHandler);
        
        systemManager.stage.window.addEventListener(
            NativeWindowBoundsEvent.MOVING, window_boundsHandler);
        
        systemManager.stage.window.addEventListener(
            NativeWindowBoundsEvent.MOVE, window_moveHandler);
        
        systemManager.stage.window.addEventListener(
            NativeWindowBoundsEvent.RESIZING, window_boundsHandler);
        /*
        systemManager.stage.window.addEventListener(
           WindowBoundsEvent.RESIZE, window_resizeHandler); */
    }

    /**
     *  @private
     */
    private function mouseMoveHandler(event:MouseEvent):void
    {
        stage.window.x += event.stageX - prevX;
        stage.window.y += event.stageY - prevY;
    }
    
    /**
     *  @private
     */ 
    private function mouseUpHandler(event:MouseEvent):void
    {
        removeEventListener(MouseEvent.MOUSE_MOVE, mouseMoveHandler);
        removeEventListener(MouseEvent.MOUSE_UP, mouseUpHandler);
    }
	/**
    *  @private
    */    
    private function titleBar_doubleClickHandler(event:MouseEvent):void
    {
		if (Capabilities.os.substring(0, 3) == "Mac")
		{
			stage.window.minimize();
		}
		else //platform is Windows
		{
			if (stage.window.displayState == NativeWindowDisplayState.MAXIMIZED)
	    		stage.window.restore();
	    	else
	    		stage.window.maximize();
		}
    }
    
    /**
     *  @private
     */ 
    private function window_boundsHandler(event:NativeWindowBoundsEvent):void
    {
  
        var newBounds:Rectangle = event.afterBounds;
        var r:Rectangle;
        if (event.type == NativeWindowBoundsEvent.MOVING)
        {
        	dispatchEvent(event);
        	if (event.isDefaultPrevented())
        		return;
        }
        else //event is resizing
        {
        	var cancel:Boolean = false;
        	if (newBounds.width < minimumWidth)
        	{	
        		cancel = true;
        		if (newBounds.x != event.beforeBounds.x && !isNaN(oldX))
        			newBounds.x = oldX;
        		newBounds.width = minimumWidth;
        	}
        	else if (newBounds.width > maximumWidth)
        	{
        		cancel = true;
        		if (newBounds.x != event.beforeBounds.x && !isNaN(oldX))
        			newBounds.x = oldX;
        		newBounds.width = maximumWidth;
        	}
        	if (newBounds.height < minimumHeight)
        	{
        		cancel = true;
        		if (event.afterBounds.y != event.beforeBounds.y && !isNaN(oldY))
 	       			newBounds.y = oldY;
        		newBounds.height = minimumHeight;
        	}
        	else if (newBounds.height > maximumHeight)
        	{
        		cancel = true;
        		if (event.afterBounds.y != event.beforeBounds.y && !isNaN(oldY))
 	       			newBounds.y = oldY;
        		newBounds.height = maximumHeight;
        	}
        	if (cancel)
        	{
        		event.preventDefault();
        		stage.window.bounds = newBounds;
        	}
        }
        oldX = newBounds.x;
        oldY = newBounds.y;
    }
            
    /**
     *  @private
     */ 
    private function window_closeEffectEndHandler(event:Event):void
    {
        removeEventListener("effectEnd", window_closeEffectEndHandler);
        stage.window.close();
    }
        
    /**
     *  @private
     */
    private function window_closingHandler(event:Event):void
    {
        var e:Event = new Event("closing", true, true);
        dispatchEvent(e);
        if (e.isDefaultPrevented())
        {
            event.preventDefault();
        }
        else if (getStyle("closeEffect") && 
                 stage.window.initOptions.transparent == true)
        {
            addEventListener("effectEnd", window_closeEffectEndHandler);
            dispatchEvent(new Event("windowClose"));
            event.preventDefault();
        } else
        {
        	stage.window.close();
        }
    }

    /**
     *  @private
     */ 
    private function window_resizeHandler(event:NativeWindowBoundsEvent):void
    {
        invalidateViewMetricsAndPadding();
        invalidateDisplayList();
        dispatchEvent(event);
        
    }
     
 	/**
 	 *  @private
 	 */
 	private function shell_activateHandler(event:Event):void
 	{
 		dispatchEvent(new ApolloEvent(ApolloEvent.APPLICATION_ACTIVATE));
 	}
 
 	/**
 	 *  @private
 	 */
 	private function shell_deactivateHandler(event:Event):void
 	{
 		dispatchEvent(new ApolloEvent(ApolloEvent.APPLICATION_DEACTIVATE));
 	}
 
 	/**
 	 *  @private
 	 */
 	private function shell_networkChangeHandler(event:Event):void
 	{
 		dispatchEvent(event);
 	} 	
 	
 	/**
 	 *  @private
 	 */
 	private var ucCount:Number = 0;
	
	/**
	 *  This is a temporary event handler which dispatches a initialLayoutComplete event after
	 *  two updateCompletes. This event will only be dispatched after either setting the bounds or
	 *  maximizing the window at startup.
	 */


	private function updateComplete_handler(event:Event):void
	{
	if (ucCount == 1)
	{
		dispatchEvent(new Event("initialLayoutComplete"));
		removeEventListener(FlexEvent.UPDATE_COMPLETE, updateComplete_handler);
	} 
	ucCount++;
	}
}

}
