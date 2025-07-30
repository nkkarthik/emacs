package com.github.nkkarthik.k

import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.actionSystem.CommonDataKeys
import org.jetbrains.plugins.terminal.TerminalToolWindowManager


class KAction : AnAction() {

    override fun actionPerformed(e: AnActionEvent) {
        val project = e.project ?: return
        val file = e.dataContext.getData(CommonDataKeys.VIRTUAL_FILE) ?: return
        val wd = file.parent ?: return

        val editor = e.getData(CommonDataKeys.EDITOR) ?: return
        val text = editor.selectionModel.selectedText
            ?: lineAt(editor.document.text, editor.selectionModel.selectionStart)

        val title = file.name

        val terminalManager = TerminalToolWindowManager.getInstance(project)
        var term = terminalManager.terminalWidgets.find { it.terminalTitle.defaultTitle == title }
        if (term == null) {
            term = terminalManager.createShellWidget(
                wd.name,
                file.name,
                true,
                true,
            )
        }
        try {
            term.sendCommandToExecute(text)
        } catch (e: Exception) {
            println("$e")
        }
        if (!terminalManager.toolWindow.isVisible) {
            terminalManager.toolWindow.show()
        }
        terminalManager.toolWindow.contentManager.contents.forEach { content ->
            val w = TerminalToolWindowManager.findWidgetByContent(content)
            if (w == term) {
                terminalManager.toolWindow.contentManager.setSelectedContent(content, false)
            }
        }

        editor.caretModel.moveCaretRelatively(0, 1, false, false, false)
    }

}


fun lineAt(body: String, offset: Int): String {
    if (body.isEmpty() || offset > body.length) {
        return ""
    }

    var start = offset
    var end = offset + 1

    // offset itself is newline go one step back
    if (offset == body.length || (body[start] == '\n' && start > 0)) {
        start--
        end--
    }

    // go back till newline
    while (start > 0) {
        if (body[start] == '\n') {
            if (start == 0 || body[start - 1] != '\\') {
                start++
                break
            }
        }
        start--
    }

    // go forward till newline
    while (end < body.length) {
        if (body[end] == '\n') {
            if (end == 0 || body[end - 1] != '\\') {
                break
            }
        }
        end++
    }

    return body.slice(start until end)
}

