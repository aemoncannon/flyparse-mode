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
public class UITextField extends FlexTextField implements IAutomationObject, IIMESupport,
						 IFlexModule,
						 IInvalidating, ISimpleStyleClient,
						 IToolTipManagerClient, IUIComponent {


    include "../core/Version.as";

    loadResources();

    private static var packageResources:ResourceBundle;

    private static var resourceTruncationIndicator:String;
}

}
