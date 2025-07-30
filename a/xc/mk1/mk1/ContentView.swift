//
//  ContentView.swift
//  mk1
//
//  Created by Krishna Nannuru on 7/10/25.
//

import SwiftUI

struct ContentView: View {
    @State private var url = "http://localhost:30001/?folder=/Users/knannuru/m"
     

    var body: some View {
        VStack {
            LocalhostWebView(
                url: url,
                urlMappings: [
                    "1": "http://localhost:30001/?folder=/Users/knannuru/m",
                    "2": "https://k/?folder=/home/knannuru/k",
                    "3":
                        "https://k/?workspace=/home/knannuru/k/.vscode/cisd.code-workspace"
                ]
            )
        }
    }
}

#Preview {
    ContentView()
}
