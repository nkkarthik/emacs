//
//  ContentView.swift
//  mk1
//
//  Created by Krishna Nannuru on 7/10/25.
//

import SwiftUI

struct ContentView: View {
    @State private var url = "http://localhost:30001/?folder=/Users/knannuru/m"
    @State private var showHelp = false
    
    private var url3: String
    private var url9: String
    private var url4: String

    init() {
        self.url3 = Bundle.main.object(forInfoDictionaryKey: "URL3") as! String
        self.url9 = "https://chatgpt.com"
//        self.url4 = "http://k:10003"
        self.url4 = "http://k:1111?floating_menu=false&swap_keys=false"
    }
    
    var body: some View {
        GeometryReader { geometry in
            HStack(spacing: 0) {
                
                LocalhostWebView(
                    url: url9,
                    onKeys: { keys in
                        switch keys {
                        case "9":
                            showHelp.toggle()
                        default:
                            break
                        }
                    },
                    urlMappings: [:],
                )
                .frame(width: geometry.size.width * (showHelp ? 0.30 : 0))
                
                LocalhostWebView(
                    url: url,
                    onKeys: { keys in
                        switch keys {
                        case "9":
                            showHelp.toggle()
                        default:
                            break
                        }
                    },
                    urlMappings: [
                        "1": "http://localhost:30001/?workspace=/Users/knannuru/m/.vscode/m.code-workspace",
                        "2": "https://k/?workspace=/home/knannuru/k/.vscode/k.code-workspace",
                        "3": url3,
                        "4": url4,
                    ]
                )
                .frame(width: geometry.size.width * (showHelp ? 0.70 : 1))

            }
        }
    }
}

#Preview {
    ContentView()
}
