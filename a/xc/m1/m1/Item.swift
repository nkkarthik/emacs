//
//  Item.swift
//  m1
//
//  Created by Krishna Nannuru on 7/11/25.
//

import Foundation
import SwiftData

@Model
final class Item {
    var timestamp: Date
    
    init(timestamp: Date) {
        self.timestamp = timestamp
    }
}
