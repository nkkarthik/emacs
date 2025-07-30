//
//  LocalhostWrapperApp.swift
//  mk1
//
//  Created by Krishna Nannuru on 7/10/25.
//

import SwiftUI
import WebKit

struct LocalhostWebView: NSViewRepresentable {
    let url: String
    let urlMappings: [String: String]

    func makeNSView(context: Context) -> WKWebView {
        let configuration = WKWebViewConfiguration()

        // Enable JavaScript
        configuration.preferences.javaScriptEnabled = true
        // Allow file access from file URLs (for local development)
        configuration.preferences.setValue(true, forKey: "allowFileAccessFromFileURLs")

        // Create webview
        let webview = CustomWKWebView(frame: .zero, configuration: configuration)
//        let webView = WKWebView(frame: .zero, configuration: configuration)
        webview.navigationDelegate = context.coordinator
        webview.setURLMappings(urlMappings)

        return webview
    }

    func updateNSView(_ webView: WKWebView, context: Context) {
        // load initial
        guard let url = URL(string: self.url) else { return }
        let request = URLRequest(url: url)
        webView.load(request)
    }

    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }

    class Coordinator: NSObject, WKNavigationDelegate {
        let parent: LocalhostWebView

        init(_ parent: LocalhostWebView) {
            self.parent = parent
        }

        func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: Error) {
            print("Failed to load: \(error.localizedDescription)")
        }

        func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
            // ensure webview becomes first responder after loading
            webView.becomeFirstResponder()
            print("Finished loading")
        }
        
        func webView(_ webView: WKWebView,
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

    }
}

class CustomWKWebView: WKWebView {
    private var urlMappings: [String: String] = [:]
    
    override var acceptsFirstResponder: Bool { return true }
    
    override func becomeFirstResponder() -> Bool { return true }
    
    override func keyDown(with event: NSEvent) {
        if event.modifierFlags.contains(.command) {
            if let keys = event.charactersIgnoringModifiers, keys.count == 1 {
                if let urlString = urlMappings[keys] {
                    load(URLRequest(url: URL(string: urlString)!))
                    return
                }
                switch keys {
                case "=":
                    pageZoom  = pageZoom * 1.1
                    // zoom in
//                    self.evaluateJavaScript("document.body.style.zoom = (parseFloat(document.body.style.zoom) || 1) * 1.1")
                case "-":
                    pageZoom = pageZoom * 0.9
                    // zoom out
//                    self.evaluateJavaScript("document.body.style.zoom = (parseFloat(document.body.style.zoom) || 1) / 1.1")
                case "0":
                    pageZoom = 1.0
                    // reset zoom
//                    self.evaluateJavaScript("document.body.style.zoom = 1")
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


