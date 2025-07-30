//
//  LocalhostWrapperApp.swift
//  mk1
//
//  Created by Krishna Nannuru on 7/10/25.
//

import SwiftUI
import WebKit

class CustomWKWebView: WKWebView {
    private var urlMappings: [String: String] = [:]
    
    public override var acceptsFirstResponder: Bool { return true }
    
    public override func becomeFirstResponder() -> Bool { return true }
    
    public override func keyDown(with event: NSEvent) {
        if event.modifierFlags.contains(.command) {
            if let keys = event.charactersIgnoringModifiers, keys.count == 1 {
                if let urlString = urlMappings[keys] {
                    load(URLRequest(url: URL(string: urlString)!))
                    window?.title = keys
                    return
                }
                // self.evaluateJavaScript("document.body.style.zoom = 1")
                switch keys {
                case "=":
                    // zoom in
                    pageZoom  = pageZoom * 1.1
                case "-":
                    pageZoom = pageZoom * 0.9
                    // zoom out
                case "0":
                    // reset zoom
                    pageZoom = 1.0
                default:
                    break
                }
            }
        }
        super.keyDown(with: event)
    }
    
    func setURLMapping(_ urlString: String, forKey key: String) {
        urlMappings[key] = urlString
    }
    
    func setURLMappings(_ mappings: [String: String]) {
        urlMappings = mappings
    }
}


