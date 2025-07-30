//
//  ContentView.swift
//  i1
//
//  Created by Krishna Nannuru on 7/15/25.
//

import SwiftUI
import SwiftData
import WebKit

struct ContentView: View {
    @State private var showWebView: Bool = false
    private var url13: String

    @Environment(\.modelContext) private var modelContext
    @Query private var items: [Item]
 
    init() {
        self.url13 = Bundle.main.object(forInfoDictionaryKey: "URL13") as! String
    }
    
    var body: some View {
        if !showWebView {
            NavigationSplitView {
                List {
                    ForEach(items) { item in
                        NavigationLink {
                            Text("Item at \(item.timestamp, format: Date.FormatStyle(date: .numeric, time: .standard))")
                        } label: {
                            Text(item.timestamp, format: Date.FormatStyle(date: .numeric, time: .standard))
                        }
                    }
                    .onDelete(perform: deleteItems)
                }
                .toolbar {
                    ToolbarItem(placement: .navigationBarTrailing) {
                        EditButton()
                    }
                    ToolbarItem {
                        Button(action: addItem) {
                            Label("Add Item", systemImage: "plus")
                        }
                    }
                }
            } detail: {
                Text("Select an item")
            }
        } else {
            LocalhostWebView(url: url13)
        }
    }

    private func addItem() {
        withAnimation {
            let newItem = Item(timestamp: Date())
            modelContext.insert(newItem)
        }
    }

    private func deleteItems(offsets: IndexSet) {
        withAnimation {
            for index in offsets {
                modelContext.delete(items[index])
            }
        }
        showWebView = true
    }
}

public struct LocalhostWebView: UIViewRepresentable {
    let url: String
    
    public init(url: String) {
        self.url = url
    }
    
    public func makeUIView(context: Context) -> WKWebView {
        let configuration = WKWebViewConfiguration()
        configuration.preferences.javaScriptEnabled = true
        let webview = WKWebView(frame: .zero, configuration: configuration)
        return webview
    }
    
    public func updateUIView(_ webView: WKWebView, context: Context) {
        guard let url = URL(string: self.url) else { return }
        let request = URLRequest(url: url)
        webView.load(request)
    }
}

#Preview {
    ContentView()
        .modelContainer(for: Item.self, inMemory: true)
}
