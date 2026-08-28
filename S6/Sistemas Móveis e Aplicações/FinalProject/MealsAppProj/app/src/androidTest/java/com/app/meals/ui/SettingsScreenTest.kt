package com.app.meals.ui

import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.assertIsOff
import androidx.compose.ui.test.isToggleable
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.test.ext.junit.runners.AndroidJUnit4
import com.app.meals.ui.screens.SettingsScreen
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith

@RunWith(AndroidJUnit4::class)
class SettingsScreenTest {

    @get:Rule
    val composeTestRule = createComposeRule()

    @Test
    fun `settings screen displays all developer info correctly`() {
        composeTestRule.setContent {
            SettingsScreen(
                isDarkMode = false,
                onThemeToggle = {},
                isPortrait = true
            )
        }

        composeTestRule.onNodeWithText("Settings").assertIsDisplayed()
        composeTestRule.onNodeWithText("Developed by").assertIsDisplayed()

        composeTestRule.onNodeWithText("Miguel Grilo").assertIsDisplayed()
        composeTestRule.onNodeWithText("58387").assertIsDisplayed()

        composeTestRule.onNodeWithText("Tiago Ramalho").assertIsDisplayed()
        composeTestRule.onNodeWithText("58514").assertIsDisplayed()

        composeTestRule.onNodeWithText("View GitHub Repository").assertIsDisplayed()
    }

    @Test
    fun `dark mode toggle updates its visual state when clicked`() {
        var isDarkTheme = false

        composeTestRule.setContent {
            SettingsScreen(
                isDarkMode = isDarkTheme,
                onThemeToggle = { isDarkTheme = it },
                isPortrait = true
            )
        }
        val themeSwitch = composeTestRule.onNode(isToggleable())

        themeSwitch.assertIsOff()

        themeSwitch.performClick()

        assert(isDarkTheme)
    }
}