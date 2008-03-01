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

import flash.display.MovieClip;
import flash.display.Loader;
import flash.events.Event;
import flash.system.ApplicationDomain;
import flash.system.LoaderContext;
import flash.utils.ByteArray;

/**
 *  Dispatched after the SWF asset has been fully loaded.
 *
 *  @eventType flash.events.Event.COMPLETE
 */
[Event(name="complete", type="flash.events.Event")]

/**
 *  MovieClipLoaderAsset is a subclass of the MovieClipAsset class
 *  which represents SWF files that you embed in a Flex application.
 */
public class MovieClipLoaderAsset extends MovieClipAsset
								  implements IFlexAsset, IFlexDisplayObject
{
	include "../core/Version.as";

	//--------------------------------------------------------------------------
	//
	//  Constructor
	//
	//--------------------------------------------------------------------------

	/**
	 *  Constructor.
	 */
	public function MovieClipLoaderAsset()
	{
		super();

 		var loaderContext:LoaderContext = new LoaderContext();
 		loaderContext.applicationDomain = new
 			ApplicationDomain(ApplicationDomain.currentDomain);
 
		loader = new Loader();
		loader.contentLoaderInfo.addEventListener(Event.COMPLETE,
												  completeHandler);
 		loader.loadBytes(movieClipData, loaderContext);
		addChild(loader);
	}

	//--------------------------------------------------------------------------
	//
	//  Variables
	//
	//--------------------------------------------------------------------------
	
	/**
	 *  @private
	 */
	private var loader:Loader = null;
	 
	/**
	 *  @private
	 */
	private var initialized:Boolean = false;
	
	/**
	 *  @private
	 */
	private var requestedWidth:Number;
	
	/**
	 *  @private
	 */
	private var requestedHeight:Number;
	
	/**
	 *  Backing storage for the <code>measuredWidth</code> property.
	 *  Subclasses should set this value in the constructor.
	 */
	protected var initialWidth:Number = 0;
	
	/**
	 *  Backing storage for the <code>measuredHeight</code> property.
	 *  Subclasses should set this value in the constructor.
	 */
	protected var initialHeight:Number = 0;

	//--------------------------------------------------------------------------
	//
	//  Overridden properties
	//
	//--------------------------------------------------------------------------

	//----------------------------------
	//  height
	//----------------------------------

	/**
	 *  @private
	 */
	override public function get height():Number
	{
		if (!initialized)
			return initialHeight;
		
		return super.height;
	}
	
	/**
	 *  @private
	 */
	override public function set height(value:Number):void
	{
		if (!initialized)
			requestedHeight = value;
		else
			loader.height = value;
	}
	
	//----------------------------------
	//  measuredHeight
	//----------------------------------

	/**
	 *  @private
	 *  The default height, in pixels.
	 */
	override public function get measuredHeight():Number
	{
		return initialHeight;
	}

	//----------------------------------
	//  measuredWidth
	//----------------------------------

	/**
	 *  @private
	 *  The default width, in pixels.
	 */
	override public function get measuredWidth():Number
	{
		return initialWidth;
	}
	
	//----------------------------------
	//  width
	//----------------------------------

	/**
	 *  @private
	 */
	override public function get width():Number
	{
		if (!initialized)
			return initialWidth;
		
		return super.width;
	}
	
	/**
	 *  @private
	 */
	override public function set width(value:Number):void
	{
		if (!initialized)
			requestedWidth = value;
		else
			loader.width = value;
	}

	//--------------------------------------------------------------------------
	//
	//  Properties
	//
	//--------------------------------------------------------------------------

	//----------------------------------
	//  movieClipData
	//----------------------------------

	/**
	 *  A ByteArray containing the inner content.
	 *  Overridden in subclasses.
	 */
	public function get movieClipData():ByteArray
	{
		return null;
	}

	//--------------------------------------------------------------------------
	//
	//  Event handlers
	//
	//--------------------------------------------------------------------------

	/**
	 *  @private
	 *  The event handler for the <code>complete</code> event.
	 */
	private function completeHandler(event:Event):void
	{
		initialized = true;
		
		initialWidth = loader.width;
		initialHeight = loader.height;
		
		if (!isNaN(requestedWidth))
			loader.width = requestedWidth;
		
		if (!isNaN(requestedHeight))
			loader.height = requestedHeight;
		
		// Forward the event
		dispatchEvent(event);
	}
}

}
