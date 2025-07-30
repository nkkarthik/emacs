//
//  m1App.swift
//  m1
//
//  Created by Krishna Nannuru on 7/11/25.
//

import SwiftUI
import SwiftData

@main
struct m1App: App {
    var sharedModelContainer: ModelContainer = {
        let schema = Schema([
            Item.self,
        ])
        let modelConfiguration = ModelConfiguration(schema: schema, isStoredInMemoryOnly: false)

        do {
            return try ModelContainer(for: schema, configurations: [modelConfiguration])
        } catch {
            fatalError("Could not create ModelContainer: \(error)")
        }
    }()
    
    init() {
        print("debugging")
        #if DEBUG
        if let icon = NSImage(named: "AppIconD") {
            print("setting debug icon")
            NSApplication.shared.applicationIconImage = icon
        }
        #endif
    }
    
    var body: some Scene {
        WindowGroup {
            ContentView()
        }
        .modelContainer(sharedModelContainer)
//        .windowStyle(.hiddenTitleBar)
    }
}
