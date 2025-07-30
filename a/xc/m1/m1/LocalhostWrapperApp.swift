//
//  LocalhostWrapperApp.swift
//  mk1
//
//  Created by Krishna Nannuru on 7/10/25.
//

import SwiftUI
import WebKit
import M1Library


public struct LocalhostWebView: NSViewRepresentable {
    let url: String
    let onKeys: (String) -> Void
    let urlMappings: [String: String]
    
    private var showing = ""

    public init(url: String, onKeys: @escaping (String)->Void, urlMappings: [String : String]) {
        self.url = url
        self.onKeys = onKeys
        self.urlMappings = urlMappings
    }
    
    public func makeNSView(context: Context) -> WKWebView {
        let configuration = WKWebViewConfiguration()
        
        // Enable JavaScript
        configuration.preferences.javaScriptEnabled = true
        
        // these two throw error
        //configuration.preferences.setValue(true, forKey: "allowsContentJavaScript")
        //configuration.preferences.setValue(true, forKey: "allowsContentCopying")
        
        configuration.preferences.setValue(true, forKey: "javaScriptCanAccessClipboard")
        configuration.preferences.setValue(true, forKey: "DOMPasteAllowed")
        // Allow file access from file URLs (for local development)
        configuration.preferences.setValue(true, forKey: "allowFileAccessFromFileURLs")

        
        // Create webview
        let webview = CustomWKWebView(frame: .zero, configuration: configuration)
        webview.navigationDelegate = context.coordinator
        webview.uiDelegate = context.coordinator
        webview.urlMappings = urlMappings
        webview.onKeys = onKeys
        
        if let url = URL(string: self.url) {
            let request = URLRequest(url: url)
            webview.load(request)
        }
        return webview
    }
    
    public func updateNSView(_ webView: WKWebView, context: Context) {
        // load initial
//        guard let url = URL(string: self.url) else { return }
//        let request = URLRequest(url: url)
//        webView.load(request)
    }
    
    public func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }
    
    
}

public class Coordinator: NSObject, WKNavigationDelegate, WKUIDelegate {
    let parent: LocalhostWebView

    init(_ parent: LocalhostWebView) {
        self.parent = parent
    }
    deinit {
        print("Coordinator deinitialized")
    }
    
    public func webView(_ webView: WKWebView, didStartProvisionalNavigation navigation: WKNavigation!) {
        print("Start loading ")
    }
    
    public func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: Error) {
        print("Failed to load: \(error.localizedDescription)")
    }
    
    public   func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        // ensure webview becomes first responder after loading
        webView.becomeFirstResponder()
        print("Finished loading")
    }
    
    // auth challenge https://k self signed cert
    public func webView(_ webView: WKWebView,
                        didReceive challenge: URLAuthenticationChallenge,
                        completionHandler: @escaping (URLSession.AuthChallengeDisposition, URLCredential?) -> Void)
    {
        guard let trust = challenge.protectionSpace.serverTrust else {
            return completionHandler(.performDefaultHandling, nil)
        }
        // Trust self-signed certificate for development only
        let exceptions = SecTrustCopyExceptions(trust)
        SecTrustSetExceptions(trust, exceptions)
        let credential = URLCredential(trust: trust)
        completionHandler(.useCredential, credential)
    }
    
    // navigation clicks
    public func webView(_ webView: WKWebView, createWebViewWith configuration: WKWebViewConfiguration, for navigationAction: WKNavigationAction, windowFeatures: WKWindowFeatures) -> WKWebView? {
        print("can go? \(navigationAction)")
        guard navigationAction.targetFrame == nil,
              let url = navigationAction.request.url else {
            return nil
        }
        NSWorkspace.shared.open(url)
        return nil
    }
    
    // navigation action
    //        func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction, decisionHandler: @escaping @MainActor (WKNavigationActionPolicy) -> Void) {
    //            print("can go? \(navigationAction)")
    //            decisionHandler(.allow)
    //        }
    //        func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction, decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
    //            func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction, ) -> Void) async -> WKNavigationActionPolicy {
    //            print("\(navigationAction)")
    //            if navigationAction.navigationType == .linkActivated {
    //                if let url = navigationAction.request.url {
    //                    print("cancel: open \(url)")
    ////                    UIApplication.shared.open(url, options: [:], completionHandler: nil)
    //                    NSWorkspace.shared.open(url)
    //                    decisionHandler(.cancel)
    //                    return
    //                }
    //            }
    //            print("allow")
    //            decisionHandler(.allow)
    //        }
    
    
}

class CustomWKWebView: WKWebView {
    var urlMappings: [String: String] = [:]
    var onKeys: (String) -> Void = ({_ in })
    
    private var current: String?
    
    public override var acceptsFirstResponder: Bool { return true }
    
    public override func becomeFirstResponder() -> Bool { return true }
    
    public override func keyDown(with event: NSEvent) {
        if event.modifierFlags.contains(.command) {
            if let keys = event.charactersIgnoringModifiers, keys.count == 1 {
                if let urlString = urlMappings[keys] {
                    if current != urlString {
                        current = urlString
                        load(URLRequest(url: URL(string: urlString)!))
                        window?.title = keys
                    }
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
                    onKeys(keys)
                    break
                }
            }
        }
        super.keyDown(with: event)
    }
    
    func setURLMapping(_ urlString: String, forKey key: String) {
        urlMappings[key] = urlString
    }
    
}


