package com.app.meals.ui

import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.test.ext.junit.runners.AndroidJUnit4
import com.app.meals.model.Meal
import com.app.meals.ui.screens.SuccessDetailScreen
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith

@RunWith(AndroidJUnit4::class)
class DetailsScreenTest {

    @get:Rule
    val composeTestRule = createComposeRule()

    @Test
    fun `tabs switch content correctly between ingredients instructions and video`() {
        val mockMeal = Meal(
            id = "1", name = "Sopa", ingredients = listOf("Cenoura"), measures = listOf("2"),
            instructions = "Ferver água e juntar legumes.", videoUrl = "https://youtube.com/sopa"
        )

        composeTestRule.setContent {
            SuccessDetailScreen(meal = mockMeal, onBackClick = {})
        }

        composeTestRule.onNodeWithText("Cenoura").assertIsDisplayed()

        composeTestRule.onNodeWithText("Instructions").performClick()

        composeTestRule.onNodeWithText("Ferver água e juntar legumes.").assertIsDisplayed()

        composeTestRule.onNodeWithText("Video").performClick()

        composeTestRule.onNodeWithText("Watch recipe on YouTube.").assertIsDisplayed()
    }
}